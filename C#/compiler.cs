using PhoenixCompiler;
using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace PhoenixCompiler
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Usage: PhoenixCompiler <source_file>");
                return;
            }

            string sourceFile = args[0];
            if (!File.Exists(sourceFile))
            {
                Console.WriteLine($"Error: File '{sourceFile}' not found.");
                return;
            }

            try
            {
                string source = File.ReadAllText(sourceFile);
                var compiler = new PhoenixCompiler();
                bool success = compiler.Compile(source, Path.ChangeExtension(sourceFile, ".exe"));

                if (success)
                {
                    Console.WriteLine("Compilation successful!");
                }
                else
                {
                    Console.WriteLine("Compilation failed.");
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
    }
}
using System;
using System.Collections.Generic;
using System.IO;

namespace PhoenixCompiler
{
    public class PhoenixCompiler
    {
        private Lexer lexer;
        private Parser parser;
        private SemanticAnalyzer semanticAnalyzer;
        private CodeGenerator codeGenerator;

        public PhoenixCompiler()
        {
            lexer = new Lexer();
            parser = new Parser();
            semanticAnalyzer = new SemanticAnalyzer();
            codeGenerator = new CodeGenerator();
        }

        public bool Compile(string source, string outputPath)
        {
            try
            {
                // Lexical Analysis
                var tokens = lexer.Tokenize(source);
                Console.WriteLine($"Lexical analysis complete. Found {tokens.Count} tokens.");

                // Parsing
                var ast = parser.Parse(tokens);
                Console.WriteLine("Parsing complete. AST generated.");

                // Semantic Analysis
                var semanticResult = semanticAnalyzer.Analyze(ast);
                if (!semanticResult.Success)
                {
                    foreach (var error in semanticResult.Errors)
                    {
                        Console.WriteLine($"Semantic Error: {error}");
                    }
                    return false;
                }
                Console.WriteLine("Semantic analysis complete.");

                // Code Generation
                var assembly = codeGenerator.Generate(ast, outputPath);
                Console.WriteLine($"Code generation complete. Output: {outputPath}");

                return true;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Compilation error: {ex.Message}");
                return false;
            }
        }
    }
}
using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace PhoenixCompiler
{
    public enum TokenType
    {
        // Literals
        Integer, Float, String, Character, Boolean, Null,

        // Identifiers
        Identifier,

        // Keywords - Core Types
        Int, FloatType, Bool, Char, StringType, Capsule,
        Stack, Heap, Mutex, Thread, Core,

        // Keywords - Control Flow
        If, Else, Loop, Break, Continue,
        Try, Catch, Throw, Return, Yield, Exit,

        // Keywords - Memory Management
        Aloc, Free, Delete, New, SmartPtr, Ref, Move, Copy,
        Lock, Unlock,

        // Keywords - Modular Structure
        Namespace, Module, Import, Export, Class, Struct, Enum, Union,
        Inline, Constexpr, Noexcept, Static, Virtual,

        // Keywords - Debugging
        Trace, Profile, Ping, Log, Assert, Check, Validate,
        Error, Warn, Info,

        // Keywords - Concurrency
        Join, Detach, Sleep, Wake, Atomic, Volatile, Sync,

        // Keywords - Type System
        Type, Auto, Decltype, Template,

        // Operators
        Plus, Minus, Multiply, Divide, Modulo,
        Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
        And, Or, Not,
        ScopeResolution, Dot, Arrow, Assign,

        // Delimiters
        LeftParen, RightParen, LeftBracket, RightBracket,
        LeftBrace, RightBrace, Semicolon, Comma, Colon,

        // Special
        EndOfFile, Unknown
    }

    public struct Token
    {
        public TokenType Type;
        public string Value;
        public int Line;
        public int Column;

        public Token(TokenType type, string value, int line, int column)
        {
            Type = type;
            Value = value;
            Line = line;
            Column = column;
        }
    }

    public class Lexer
    {
        private static readonly Dictionary<string, TokenType> Keywords = new Dictionary<string, TokenType>
        {
            // Core Types
            {"int", TokenType.Int}, {"float", TokenType.FloatType}, {"bool", TokenType.Bool},
            {"char", TokenType.Char}, {"string", TokenType.StringType}, {"null", TokenType.Null},
            {"capsule", TokenType.Capsule}, {"stack", TokenType.Stack}, {"heap", TokenType.Heap},
            {"mutex", TokenType.Mutex}, {"thread", TokenType.Thread}, {"core", TokenType.Core},
            
            // Control Flow
            {"if", TokenType.If}, {"else", TokenType.Else}, {"loop", TokenType.Loop},
            {"break", TokenType.Break}, {"continue", TokenType.Continue},
            {"try", TokenType.Try}, {"catch", TokenType.Catch}, {"throw", TokenType.Throw},
            {"return", TokenType.Return}, {"yield", TokenType.Yield}, {"exit", TokenType.Exit},
            
            // Memory Management
            {"aloc", TokenType.Aloc}, {"free", TokenType.Free}, {"delete", TokenType.Delete},
            {"new", TokenType.New}, {"smartptr", TokenType.SmartPtr}, {"ref", TokenType.Ref},
            {"move", TokenType.Move}, {"copy", TokenType.Copy}, {"lock", TokenType.Lock},
            {"unlock", TokenType.Unlock},
            
            // Modular Structure
            {"namespace", TokenType.Namespace}, {"module", TokenType.Module},
            {"import", TokenType.Import}, {"export", TokenType.Export},
            {"class", TokenType.Class}, {"struct", TokenType.Struct},
            {"enum", TokenType.Enum}, {"union", TokenType.Union},
            {"inline", TokenType.Inline}, {"constexpr", TokenType.Constexpr},
            {"noexcept", TokenType.Noexcept}, {"static", TokenType.Static},
            {"virtual", TokenType.Virtual},
            
            // Debugging
            {"trace", TokenType.Trace}, {"profile", TokenType.Profile},
            {"ping", TokenType.Ping}, {"log", TokenType.Log},
            {"assert", TokenType.Assert}, {"check", TokenType.Check},
            {"validate", TokenType.Validate}, {"error", TokenType.Error},
            {"warn", TokenType.Warn}, {"info", TokenType.Info},
            
            // Concurrency
            {"join", TokenType.Join}, {"detach", TokenType.Detach},
            {"sleep", TokenType.Sleep}, {"wake", TokenType.Wake},
            {"atomic", TokenType.Atomic}, {"volatile", TokenType.Volatile},
            {"sync", TokenType.Sync},
            
            // Type System
            {"type", TokenType.Type}, {"auto", TokenType.Auto},
            {"decltype", TokenType.Decltype}, {"template", TokenType.Template},
            
            // Literals
            {"true", TokenType.Boolean}, {"false", TokenType.Boolean}
        };

        private string source;
        private int position;
        private int line;
        private int column;

        public List<Token> Tokenize(string source)
        {
            this.source = source;
            position = 0;
            line = 1;
            column = 1;

            var tokens = new List<Token>();

            while (position < source.Length)
            {
                SkipWhitespace();

                if (position >= source.Length)
                    break;

                var token = NextToken();
                if (token.Type != TokenType.Unknown)
                    tokens.Add(token);
            }

            tokens.Add(new Token(TokenType.EndOfFile, "", line, column));
            return tokens;
        }

        private void SkipWhitespace()
        {
            while (position < source.Length && char.IsWhiteSpace(source[position]))
            {
                if (source[position] == '\n')
                {
                    line++;
                    column = 1;
                }
                else
                {
                    column++;
                }
                position++;
            }
        }

        private Token NextToken()
        {
            if (position >= source.Length)
                return new Token(TokenType.EndOfFile, "", line, column);

            int startLine = line;
            int startColumn = column;
            char current = source[position];

            // Comments
            if (current == '/' && Peek() == '/')
            {
                SkipLineComment();
                return NextToken();
            }

            if (current == '/' && Peek() == '*')
            {
                SkipBlockComment();
                return NextToken();
            }

            // String literals
            if (current == '"')
            {
                return ReadString();
            }

            // Character literals
            if (current == '\'')
            {
                return ReadCharacter();
            }

            // Numbers
            if (char.IsDigit(current))
            {
                return ReadNumber();
            }

            // Identifiers and keywords
            if (char.IsLetter(current) || current == '_')
            {
                return ReadIdentifier();
            }

            // Two-character operators
            if (position + 1 < source.Length)
            {
                string twoChar = source.Substring(position, 2);
                switch (twoChar)
                {
                    case "::": Advance(2); return new Token(TokenType.ScopeResolution, "::", startLine, startColumn);
                    case "->": Advance(2); return new Token(TokenType.Arrow, "->", startLine, startColumn);
                    case "==": Advance(2); return new Token(TokenType.Equal, "==", startLine, startColumn);
                    case "!=": Advance(2); return new Token(TokenType.NotEqual, "!=", startLine, startColumn);
                    case "<=": Advance(2); return new Token(TokenType.LessEqual, "<=", startLine, startColumn);
                    case ">=": Advance(2); return new Token(TokenType.GreaterEqual, ">=", startLine, startColumn);
                    case "&&": Advance(2); return new Token(TokenType.And, "&&", startLine, startColumn);
                    case "||": Advance(2); return new Token(TokenType.Or, "||", startLine, startColumn);
                }
            }

            // Single-character tokens
            switch (current)
            {
                case '+': Advance(); return new Token(TokenType.Plus, "+", startLine, startColumn);
                case '-': Advance(); return new Token(TokenType.Minus, "-", startLine, startColumn);
                case '*': Advance(); return new Token(TokenType.Multiply, "*", startLine, startColumn);
                case '/': Advance(); return new Token(TokenType.Divide, "/", startLine, startColumn);
                case '%': Advance(); return new Token(TokenType.Modulo, "%", startLine, startColumn);
                case '<': Advance(); return new Token(TokenType.Less, "<", startLine, startColumn);
                case '>': Advance(); return new Token(TokenType.Greater, ">", startLine, startColumn);
                case '!': Advance(); return new Token(TokenType.Not, "!", startLine, startColumn);
                case '=': Advance(); return new Token(TokenType.Assign, "=", startLine, startColumn);
                case '.': Advance(); return new Token(TokenType.Dot, ".", startLine, startColumn);
                case '(': Advance(); return new Token(TokenType.LeftParen, "(", startLine, startColumn);
                case ')': Advance(); return new Token(TokenType.RightParen, ")", startLine, startColumn);
                case '[': Advance(); return new Token(TokenType.LeftBracket, "[", startLine, startColumn);
                case ']': Advance(); return new Token(TokenType.RightBracket, "]", startLine, startColumn);
                case '{': Advance(); return new Token(TokenType.LeftBrace, "{", startLine, startColumn);
                case '}': Advance(); return new Token(TokenType.RightBrace, "}", startLine, startColumn);
                case ';': Advance(); return new Token(TokenType.Semicolon, ";", startLine, startColumn);
                case ',': Advance(); return new Token(TokenType.Comma, ",", startLine, startColumn);
                case ':': Advance(); return new Token(TokenType.Colon, ":", startLine, startColumn);
                default:
                    Advance();
                    return new Token(TokenType.Unknown, current.ToString(), startLine, startColumn);
            }
        }

        private void SkipLineComment()
        {
            while (position < source.Length && source[position] != '\n')
            {
                position++;
                column++;
            }
        }

        private void SkipBlockComment()
        {
            position += 2; // Skip /*
            column += 2;

            while (position + 1 < source.Length)
            {
                if (source[position] == '*' && source[position + 1] == '/')
                {
                    position += 2;
                    column += 2;
                    break;
                }

                if (source[position] == '\n')
                {
                    line++;
                    column = 1;
                }
                else
                {
                    column++;
                }
                position++;
            }
        }

        private Token ReadString()
        {
            int startLine = line;
            int startColumn = column;
            var sb = new StringBuilder();

            Advance(); // Skip opening quote

            while (position < source.Length && source[position] != '"')
            {
                if (source[position] == '\\' && position + 1 < source.Length)
                {
                    position++;
                    column++;
                    char escaped = source[position];
                    switch (escaped)
                    {
                        case 'n': sb.Append('\n'); break;
                        case 't': sb.Append('\t'); break;
                        case 'r': sb.Append('\r'); break;
                        case '\\': sb.Append('\\'); break;
                        case '"': sb.Append('"'); break;
                        default: sb.Append(escaped); break;
                    }
                }
                else
                {
                    sb.Append(source[position]);
                }

                Advance();
            }

            if (position < source.Length)
                Advance(); // Skip closing quote

            return new Token(TokenType.String, sb.ToString(), startLine, startColumn);
        }

        private Token ReadCharacter()
        {
            int startLine = line;
            int startColumn = column;

            Advance(); // Skip opening quote
            char value = source[position];
            Advance();

            if (position < source.Length && source[position] == '\'')
                Advance(); // Skip closing quote

            return new Token(TokenType.Character, value.ToString(), startLine, startColumn);
        }

        private Token ReadNumber()
        {
            int startLine = line;
            int startColumn = column;
            var sb = new StringBuilder();
            bool isFloat = false;

            while (position < source.Length && (char.IsDigit(source[position]) || source[position] == '.'))
            {
                if (source[position] == '.')
                {
                    if (isFloat) break; // Second dot, stop
                    isFloat = true;
                }
                sb.Append(source[position]);
                Advance();
            }

            return new Token(isFloat ? TokenType.Float : TokenType.Integer, sb.ToString(), startLine, startColumn);
        }

        private Token ReadIdentifier()
        {
            int startLine = line;
            int startColumn = column;
            var sb = new StringBuilder();

            while (position < source.Length &&
                   (char.IsLetterOrDigit(source[position]) || source[position] == '_'))
            {
                sb.Append(source[position]);
                Advance();
            }

            string value = sb.ToString();
            TokenType type = Keywords.ContainsKey(value) ? Keywords[value] : TokenType.Identifier;

            return new Token(type, value, startLine, startColumn);
        }

        private char Peek()
        {
            return position + 1 < source.Length ? source[position + 1] : '\0';
        }

        private void Advance(int count = 1)
        {
            for (int i = 0; i < count && position < source.Length; i++)
            {
                position++;
                column++;
            }
        }
    }
}
using System;
using System.Collections.Generic;

namespace PhoenixCompiler
{
    // Base AST Node
    public abstract class ASTNode
    {
        public int Line { get; set; }
        public int Column { get; set; }
    }

    // Program (Root node)
    public class ProgramNode : ASTNode
    {
        public List<ASTNode> Declarations { get; set; } = new List<ASTNode>();
    }

    // Declarations
    public abstract class DeclarationNode : ASTNode { }

    public class CapsuleDeclarationNode : DeclarationNode
    {
        public string Name { get; set; }
        public List<DeclarationNode> Members { get; set; } = new List<DeclarationNode>();
        public List<string> BaseClasses { get; set; } = new List<string>();
    }

    public class FunctionDeclarationNode : DeclarationNode
    {
        public string Name { get; set; }
        public TypeNode ReturnType { get; set; }
        public List<ParameterNode> Parameters { get; set; } = new List<ParameterNode>();
        public BlockStatementNode Body { get; set; }
        public bool IsVirtual { get; set; }
        public bool IsStatic { get; set; }
        public bool IsInline { get; set; }
        public bool IsConstexpr { get; set; }
        public bool IsNoexcept { get; set; }
    }

    public class VariableDeclarationNode : DeclarationNode
    {
        public string Name { get; set; }
        public TypeNode Type { get; set; }
        public ExpressionNode Initializer { get; set; }
        public bool IsStatic { get; set; }
        public bool IsVolatile { get; set; }
        public bool IsAtomic { get; set; }
    }

    public class NamespaceDeclarationNode : DeclarationNode
    {
        public string Name { get; set; }
        public List<DeclarationNode> Members { get; set; } = new List<DeclarationNode>();
    }

    public class StructDeclarationNode : DeclarationNode
    {
        public string Name { get; set; }
        public List<VariableDeclarationNode> Fields { get; set; } = new List<VariableDeclarationNode>();
    }

    public class EnumDeclarationNode : DeclarationNode
    {
        public string Name { get; set; }
        public List<string> Values { get; set; } = new List<string>();
    }

    // Parameters
    public class ParameterNode : ASTNode
    {
        public string Name { get; set; }
        public TypeNode Type { get; set; }
        public ExpressionNode DefaultValue { get; set; }
    }

    // Types
    public abstract class TypeNode : ASTNode { }

    public class PrimitiveTypeNode : TypeNode
    {
        public string Name { get; set; } // int, float, bool, char, string, etc.
    }

    public class PointerTypeNode : TypeNode
    {
        public TypeNode BaseType { get; set; }
    }

    public class ReferenceTypeNode : TypeNode
    {
        public TypeNode BaseType { get; set; }
    }

    public class ArrayTypeNode : TypeNode
    {
        public TypeNode ElementType { get; set; }
        public ExpressionNode Size { get; set; }
    }

    // Statements
    public abstract class StatementNode : ASTNode { }

    public class BlockStatementNode : StatementNode
    {
        public List<StatementNode> Statements { get; set; } = new List<StatementNode>();
    }

    public class ExpressionStatementNode : StatementNode
    {
        public ExpressionNode Expression { get; set; }
    }

    public class IfStatementNode : StatementNode
    {
        public ExpressionNode Condition { get; set; }
        public StatementNode ThenStatement { get; set; }
        public StatementNode ElseStatement { get; set; }
    }

    public class LoopStatementNode : StatementNode
    {
        public ExpressionNode Condition { get; set; }
        public StatementNode Body { get; set; }
    }

    public class ReturnStatementNode : StatementNode
    {
        public ExpressionNode Expression { get; set; }
    }

    public class BreakStatementNode : StatementNode { }

    public class ContinueStatementNode : StatementNode { }

    public class TryStatementNode : StatementNode
    {
        public BlockStatementNode TryBlock { get; set; }
        public List<CatchClauseNode> CatchClauses { get; set; } = new List<CatchClauseNode>();
    }

    public class CatchClauseNode : ASTNode
    {
        public TypeNode ExceptionType { get; set; }
        public string VariableName { get; set; }
        public BlockStatementNode Body { get; set; }
    }

    public class ThrowStatementNode : StatementNode
    {
        public ExpressionNode Expression { get; set; }
    }

    // Expressions
    public abstract class ExpressionNode : ASTNode { }

    public class LiteralExpressionNode : ExpressionNode
    {
        public object Value { get; set; }
        public TokenType Type { get; set; }
    }

    public class IdentifierExpressionNode : ExpressionNode
    {
        public string Name { get; set; }
    }

    public class BinaryExpressionNode : ExpressionNode
    {
        public ExpressionNode Left { get; set; }
        public TokenType Operator { get; set; }
        public ExpressionNode Right { get; set; }
    }

    public class UnaryExpressionNode : ExpressionNode
    {
        public TokenType Operator { get; set; }
        public ExpressionNode Operand { get; set; }
    }

    public class FunctionCallExpressionNode : ExpressionNode
    {
        public ExpressionNode Function { get; set; }
        public List<ExpressionNode> Arguments { get; set; } = new List<ExpressionNode>();
    }

    public class MemberAccessExpressionNode : ExpressionNode
    {
        public ExpressionNode Object { get; set; }
        public string MemberName { get; set; }
        public bool IsArrowAccess { get; set; } // -> vs .
    }

    public class ArrayAccessExpressionNode : ExpressionNode
    {
        public ExpressionNode Array { get; set; }
        public ExpressionNode Index { get; set; }
    }

    public class AssignmentExpressionNode : ExpressionNode
    {
        public ExpressionNode Left { get; set; }
        public ExpressionNode Right { get; set; }
    }

    public class NewExpressionNode : ExpressionNode
    {
        public TypeNode Type { get; set; }
        public List<ExpressionNode> Arguments { get; set; } = new List<ExpressionNode>();
    }
}
using System;
using System.Collections.Generic;
using System.Linq;

namespace PhoenixCompiler
{
    public class Parser
    {
        private List<Token> tokens;
        private int position;

        public ProgramNode Parse(List<Token> tokens)
        {
            this.tokens = tokens;
            position = 0;

            var program = new ProgramNode();

            while (!IsAtEnd())
            {
                var declaration = ParseDeclaration();
                if (declaration != null)
                    program.Declarations.Add(declaration);
            }

            return program;
        }

        private DeclarationNode ParseDeclaration()
        {
            try
            {
                if (Check(TokenType.Capsule))
                    return ParseCapsuleDeclaration();
                if (Check(TokenType.Namespace))
                    return ParseNamespaceDeclaration();
                if (Check(TokenType.Struct))
                    return ParseStructDeclaration();
                if (Check(TokenType.Enum))
                    return ParseEnumDeclaration();

                // Check for function or variable declaration
                var checkpoint = position;
                try
                {
                    var type = ParseType();
                    var name = Consume(TokenType.Identifier, "Expected identifier").Value;

                    if (Check(TokenType.LeftParen))
                    {
                        // Function declaration
                        position = checkpoint;
                        return ParseFunctionDeclaration();
                    }
                    else
                    {
                        // Variable declaration
                        position = checkpoint;
                        return ParseVariableDeclaration();
                    }
                }
                catch
                {
                    position = checkpoint;
                    throw;
                }
            }
            catch (Exception ex)
            {
                // Error recovery: skip to next statement
                Console.WriteLine($"Parse error: {ex.Message}");
                while (!IsAtEnd() && !Check(TokenType.Semicolon))
                    Advance();
                if (Match(TokenType.Semicolon)) { }
                return null;
            }
        }

        private CapsuleDeclarationNode ParseCapsuleDeclaration()
        {
            var capsule = new CapsuleDeclarationNode();
            SetLocation(capsule);

            Consume(TokenType.Capsule, "Expected 'capsule'");
            capsule.Name = Consume(TokenType.Identifier, "Expected capsule name").Value;

            // Inheritance (optional)
            if (Match(TokenType.Colon))
            {
                do
                {
                    capsule.BaseClasses.Add(Consume(TokenType.Identifier, "Expected base class name").Value);
                } while (Match(TokenType.Comma));
            }

            Consume(TokenType.LeftBrace, "Expected '{'");

            while (!Check(TokenType.RightBrace) && !IsAtEnd())
            {
                var member = ParseDeclaration();
                if (member != null)
                    capsule.Members.Add(member);
            }

            Consume(TokenType.RightBrace, "Expected '}'");
            return capsule;
        }

        private FunctionDeclarationNode ParseFunctionDeclaration()
        {
            var function = new FunctionDeclarationNode();
            SetLocation(function);

            // Parse modifiers
            while (Match(TokenType.Static, TokenType.Virtual, TokenType.Inline,
                        TokenType.Constexpr, TokenType.Noexcept))
            {
                switch (Previous().Type)
                {
                    case TokenType.Static: function.IsStatic = true; break;
                    case TokenType.Virtual: function.IsVirtual = true; break;
                    case TokenType.Inline: function.IsInline = true; break;
                    case TokenType.Constexpr: function.IsConstexpr = true; break;
                    case TokenType.Noexcept: function.IsNoexcept = true; break;
                }
            }

            function.ReturnType = ParseType();
            function.Name = Consume(TokenType.Identifier, "Expected function name").Value;

            Consume(TokenType.LeftParen, "Expected '('");

            // Parameters
            if (!Check(TokenType.RightParen))
            {
                do
                {
                    var param = new ParameterNode();
                    SetLocation(param);
                    param.Type = ParseType();
                    param.Name = Consume(TokenType.Identifier, "Expected parameter name").Value;

                    if (Match(TokenType.Assign))
                    {
                        param.DefaultValue = ParseExpression();
                    }

                    function.Parameters.Add(param);
                } while (Match(TokenType.Comma));
            }

            Consume(TokenType.RightParen, "Expected ')'");

            if (Check(TokenType.LeftBrace))
            {
                function.Body = ParseBlockStatement();
            }
            else
            {
                Consume(TokenType.Semicolon, "Expected ';' for function declaration");
            }

            return function;
        }

        private VariableDeclarationNode ParseVariableDeclaration()
        {
            var variable = new VariableDeclarationNode();
            SetLocation(variable);

            // Parse modifiers
            while (Match(TokenType.Static, TokenType.Volatile, TokenType.Atomic))
            {
                switch (Previous().Type)
                {
                    case TokenType.Static: variable.IsStatic = true; break;
                    case TokenType.Volatile: variable.IsVolatile = true; break;
                    case TokenType.Atomic: variable.IsAtomic = true; break;
                }
            }

            variable.Type = ParseType();
            variable.Name = Consume(TokenType.Identifier, "Expected variable name").Value;

            if (Match(TokenType.Assign))
            {
                variable.Initializer = ParseExpression();
            }

            Consume(TokenType.Semicolon, "Expected ';'");
            return variable;
        }

        private NamespaceDeclarationNode ParseNamespaceDeclaration()
        {
            var ns = new NamespaceDeclarationNode();
            SetLocation(ns);

            Consume(TokenType.Namespace, "Expected 'namespace'");
            ns.Name = Consume(TokenType.Identifier, "Expected namespace name").Value;
            Consume(TokenType.LeftBrace, "Expected '{'");

            while (!Check(TokenType.RightBrace) && !IsAtEnd())
            {
                var member = ParseDeclaration();
                if (member != null)
                    ns.Members.Add(member);
            }

            Consume(TokenType.RightBrace, "Expected '}'");
            return ns;
        }

        private StructDeclarationNode ParseStructDeclaration()
        {
            var structNode = new StructDeclarationNode();
            SetLocation(structNode);

            Consume(TokenType.Struct, "Expected 'struct'");
            structNode.Name = Consume(TokenType.Identifier, "Expected struct name").Value;
            Consume(TokenType.LeftBrace, "Expected '{'");

            while (!Check(TokenType.RightBrace) && !IsAtEnd())
            {
                var field = ParseVariableDeclaration();
                if (field != null)
                    structNode.Fields.Add(field);
            }

            Consume(TokenType.RightBrace, "Expected '}'");
            Consume(TokenType.Semicolon, "Expected ';'");
            return structNode;
        }

        private EnumDeclarationNode ParseEnumDeclaration()
        {
            var enumNode = new EnumDeclarationNode();
            SetLocation(enumNode);

            Consume(TokenType.Enum, "Expected 'enum'");
            enumNode.Name = Consume(TokenType.Identifier, "Expected enum name").Value;
            Consume(TokenType.LeftBrace, "Expected '{'");

            if (!Check(TokenType.RightBrace))
            {
                do
                {
                    enumNode.Values.Add(Consume(TokenType.Identifier, "Expected enum value").Value);
                } while (Match(TokenType.Comma));
            }

            Consume(TokenType.RightBrace, "Expected '}'");
            Consume(TokenType.Semicolon, "Expected ';'");
            return enumNode;
        }

        private TypeNode ParseType()
        {
            if (Match(TokenType.Int, TokenType.FloatType, TokenType.Bool,
                     TokenType.Char, TokenType.StringType, TokenType.Null))
            {
                var primitive = new PrimitiveTypeNode();
                SetLocation(primitive);
                primitive.Name = Previous().Value;
                return primitive;
            }

            if (Match(TokenType.Identifier))
            {
                var primitive = new PrimitiveTypeNode();
                SetLocation(primitive);
                primitive.Name = Previous().Value;
                return primitive;
            }

            throw new Exception("Expected type");
        }

        private BlockStatementNode ParseBlockStatement()
        {
            var block = new BlockStatementNode();
            SetLocation(block);

            Consume(TokenType.LeftBrace, "Expected '{'");

            while (!Check(TokenType.RightBrace) && !IsAtEnd())
            {
                var statement = ParseStatement();
                if (statement != null)
                    block.Statements.Add(statement);
            }

            Consume(TokenType.RightBrace, "Expected '}'");
            return block;
        }

        private StatementNode ParseStatement()
        {
            try
            {
                if (Match(TokenType.If))
                    return ParseIfStatement();
                if (Match(TokenType.Loop))
                    return ParseLoopStatement();
                if (Match(TokenType.Return))
                    return ParseReturnStatement();
                if (Match(TokenType.Break))
                    return ParseBreakStatement();
                if (Match(TokenType.Continue))
                    return ParseContinueStatement();
                if (Match(TokenType.Try))
                    return ParseTryStatement();
                if (Match(TokenType.Throw))
                    return ParseThrowStatement();
                if (Check(TokenType.LeftBrace))
                    return ParseBlockStatement();

                // Expression statement
                var expr = ParseExpression();
                Consume(TokenType.Semicolon, "Expected ';'");
                var exprStmt = new ExpressionStatementNode();
                SetLocation(exprStmt);
                exprStmt.Expression = expr;
                return exprStmt;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Statement parse error: {ex.Message}");
                // Error recovery
                while (!IsAtEnd() && !Check(TokenType.Semicolon, TokenType.RightBrace))
                    Advance();
                if (Match(TokenType.Semicolon)) { }
                return null;
            }
        }

        private IfStatementNode ParseIfStatement()
        {
            var ifStmt = new IfStatementNode();
            SetLocation(ifStmt);

            Consume(TokenType.LeftParen, "Expected '(' after 'if'");
            ifStmt.Condition = ParseExpression();
            Consume(TokenType.RightParen, "Expected ')'");
            ifStmt.ThenStatement = ParseStatement();

            if (Match(TokenType.Else))
            {
                ifStmt.ElseStatement = ParseStatement();
            }

            return ifStmt;
        }

        private LoopStatementNode ParseLoopStatement()
        {
            var loop = new LoopStatementNode();
            SetLocation(loop);

            Consume(TokenType.LeftParen, "Expected '(' after 'loop'");
            loop.Condition = ParseExpression();
            Consume(TokenType.RightParen, "Expected ')'");
            loop.Body = ParseStatement();

            return loop;
        }

        private ReturnStatementNode ParseReturnStatement()
        {
            var returnStmt = new ReturnStatementNode();
            SetLocation(returnStmt);

            if (!Check(TokenType.Semicolon))
            {
                returnStmt.Expression = ParseExpression();
            }

            Consume(TokenType.Semicolon, "Expected ';'");
            return returnStmt;
        }

        private BreakStatementNode ParseBreakStatement()
        {
            var breakStmt = new BreakStatementNode();
            SetLocation(breakStmt);
            Consume(TokenType.Semicolon, "Expected ';'");
            return breakStmt;
        }

        private ContinueStatementNode ParseContinueStatement()
        {
            var continueStmt = new ContinueStatementNode();
            SetLocation(continueStmt);
            Consume(TokenType.Semicolon, "Expected ';'");
            return continueStmt;
        }

        private TryStatementNode ParseTryStatement()
        {
            var tryStmt = new TryStatementNode();
            SetLocation(tryStmt);

            tryStmt.TryBlock = ParseBlockStatement();

            while (Match(TokenType.Catch))
            {
                var catchClause = new CatchClauseNode();
                SetLocation(catchClause);

                Consume(TokenType.LeftParen, "Expected '(' after 'catch'");
                catchClause.ExceptionType = ParseType();
                catchClause.VariableName = Consume(TokenType.Identifier, "Expected variable name").Value;
                Consume(TokenType.RightParen, "Expected ')'");
                catchClause.Body = ParseBlockStatement();

                tryStmt.CatchClauses.Add(catchClause);
            }

            return tryStmt;
        }

        private ThrowStatementNode ParseThrowStatement()
        {
            var throwStmt = new ThrowStatementNode();
            SetLocation(throwStmt);

            if (!Check(TokenType.Semicolon))
            {
                throwStmt.Expression = ParseExpression();
            }

            Consume(TokenType.Semicolon, "Expected ';'");
            return throwStmt;
        }

        private ExpressionNode ParseExpression()
        {
            return ParseAssignment();
        }

        private ExpressionNode ParseAssignment()
        {
            var expr = ParseLogicalOr();

            if (Match(TokenType.Assign))
            {
                var assignment = new AssignmentExpressionNode();
                SetLocation(assignment);
                assignment.Left = expr;
                assignment.Right = ParseAssignment();
                return assignment;
            }

            return expr;
        }

        private ExpressionNode ParseLogicalOr()
        {
            var expr = ParseLogicalAnd();

            while (Match(TokenType.Or))
            {
                var binary = new BinaryExpressionNode();
                SetLocation(binary);
                binary.Left = expr;
                binary.Operator = Previous().Type;
                binary.Right = ParseLogicalAnd();
                expr = binary;
            }

            return expr;
        }

        private ExpressionNode ParseLogicalAnd()
        {
            var expr = ParseEquality();

            while (Match(TokenType.And))
            {
                var binary = new BinaryExpressionNode();
                SetLocation(binary);
                binary.Left = expr;
                binary.Operator = Previous().Type;
                binary.Right = ParseEquality();
                expr = binary;
            }

            return expr;
        }

        private ExpressionNode ParseEquality()
        {
            var expr = ParseComparison();

            while (Match(TokenType.Equal, TokenType.NotEqual))
            {
                var binary = new BinaryExpressionNode();
                SetLocation(binary);
                binary.Left = expr;
                binary.Operator = Previous().Type;
                binary.Right = ParseComparison();
                expr = binary;
            }

            return expr;
        }

        private ExpressionNode ParseComparison()
        {
            var expr = ParseTerm();

            while (Match(TokenType.Greater, TokenType.GreaterEqual,
                        TokenType.Less, TokenType.LessEqual))
            {
                var binary = new BinaryExpressionNode();
                SetLocation(binary);
                binary.Left = expr;
                binary.Operator = Previous().Type;
                binary.Right = ParseTerm();
                expr = binary;
            }

            return expr;
        }

        private ExpressionNode ParseTerm()
        {
            var expr = ParseFactor();

            while (Match(TokenType.Minus, TokenType.Plus))
            {
                var binary = new BinaryExpressionNode();
                SetLocation(binary);
                binary.Left = expr;
                binary.Operator = Previous().Type;
                binary.Right = ParseFactor();
                expr = binary;
            }

            return expr;
        }

        private ExpressionNode ParseFactor()
        {
            var expr = ParseUnary();

            while (Match(TokenType.Divide, TokenType.Multiply, TokenType.Modulo))
            {
                var binary = new BinaryExpressionNode();
                SetLocation(binary);
                binary.Left = expr;
                binary.Operator = Previous().Type;
                binary.Right = ParseUnary();
                expr = binary;
            }

            return expr;
        }

        private ExpressionNode ParseUnary()
        {
            if (Match(TokenType.Not, TokenType.Minus))
            {
                var unary = new UnaryExpressionNode();
                SetLocation(unary);
                unary.Operator = Previous().Type;
                unary.Operand = ParseUnary();
                return unary;
            }

            return ParseCall();
        }

        private ExpressionNode ParseCall()
        {
            var expr = ParsePrimary();

            while (true)
            {
                if (Match(TokenType.LeftParen))
                {
                    var call = new FunctionCallExpressionNode();
                    SetLocation(call);
                    call.Function = expr;

                    if (!Check(TokenType.RightParen))
                    {
                        do
                        {
                            call.Arguments.Add(ParseExpression());
                        } while (Match(TokenType.Comma));
                    }

                    Consume(TokenType.RightParen, "Expected ')'");
                    expr = call;
                }
                else if (Match(TokenType.Dot, TokenType.Arrow))
                {
                    var member = new MemberAccessExpressionNode();
                    SetLocation(member);
                    member.Object = expr;
                    member.IsArrowAccess = Previous().Type == TokenType.Arrow;
                    member.MemberName = Consume(TokenType.Identifier, "Expected member name").Value;
                    expr = member;
                }
                else if (Match(TokenType.LeftBracket))
                {
                    var arrayAccess = new ArrayAccessExpressionNode();
                    SetLocation(arrayAccess);
                    arrayAccess.Array = expr;
                    arrayAccess.Index = ParseExpression();
                    Consume(TokenType.RightBracket, "Expected ']'");
                    expr = arrayAccess;
                }
                else
                {
                    break;
                }
            }

            return expr;
        }

        private ExpressionNode ParsePrimary()
        {
            if (Match(TokenType.Boolean, TokenType.Null))
            {
                var literal = new LiteralExpressionNode();
                SetLocation(literal);
                literal.Type = Previous().Type;
                literal.Value = Previous().Value;
                return literal;
            }

            if (Match(TokenType.Integer))
            {
                var literal = new LiteralExpressionNode();
                SetLocation(literal);
                literal.Type = TokenType.Integer;
                literal.Value = int.Parse(Previous().Value);
                return literal;
            }

            if (Match(TokenType.Float))
            {
                var literal = new LiteralExpressionNode();
                SetLocation(literal);
                literal.Type = TokenType.Float;
                literal.Value = double.Parse(Previous().Value);
                return literal;
            }

            if (Match(TokenType.String))
            {
                var literal = new LiteralExpressionNode();
                SetLocation(literal);
                literal.Type = TokenType.String;
                literal.Value = Previous().Value;
                return literal;
            }

            if (Match(TokenType.Character))
            {
                var literal = new LiteralExpressionNode();
                SetLocation(literal);
                literal.Type = TokenType.Character;
                literal.Value = Previous().Value[0];
                return literal;
            }

            if (Match(TokenType.Identifier))
            {
                var identifier = new IdentifierExpressionNode();
                SetLocation(identifier);
                identifier.Name = Previous().Value;
                return identifier;
            }

            if (Match(TokenType.New))
            {
                var newExpr = new NewExpressionNode();
                SetLocation(newExpr);
                newExpr.Type = ParseType();

                if (Match(TokenType.LeftParen))
                {
                    if (!Check(TokenType.RightParen))
                    {
                        do
                        {
                            newExpr.Arguments.Add(ParseExpression());
                        } while (Match(TokenType.Comma));
                    }
                    Consume(TokenType.RightParen, "Expected ')'");
                }

                return newExpr;
            }

            if (Match(TokenType.LeftParen))
            {
                var expr = ParseExpression();
                Consume(TokenType.RightParen, "Expected ')'");
                return expr;
            }

            throw new Exception($"Unexpected token: {Peek().Value}");
        }

        // Helper methods
        private bool Match(params TokenType[] types)
        {
            foreach (var type in types)
            {
                if (Check(type))
                {
                    Advance();
                    return true;
                }
            }
            return false;
        }

        private bool Check(params TokenType[] types)
        {
            if (IsAtEnd()) return false;
            return types.Contains(Peek().Type);
        }

        private Token Advance()
        {
            if (!IsAtEnd()) position++;
            return Previous();
        }

        private bool IsAtEnd()
        {
            return Peek().Type == TokenType.EndOfFile;
        }

        private Token Peek()
        {
            return tokens[position];
        }

        private Token Previous()
        {
            return tokens[position - 1];
        }

        private Token Consume(TokenType type, string message)
        {
            if (Check(type)) return Advance();
            throw new Exception($"{message}. Got {Peek().Type} at line {Peek().Line}");
        }

        private void SetLocation(ASTNode node)
        {
            if (position < tokens.Count)
            {
                node.Line = tokens[position].Line;
                node.Column = tokens[position].Column;
            }
        }
    }
}
using System;
using System.Collections.Generic;
using System.Linq;

namespace PhoenixCompiler
{
    public class SemanticResult
    {
        public bool Success { get; set; }
        public List<string> Errors { get; set; } = new List<string>();
    }

    public class Symbol
    {
        public string Name { get; set; }
        public string Type { get; set; }
        public bool IsFunction { get; set; }
        public bool IsVariable { get; set; }
        public bool IsType { get; set; }
        public int Line { get; set; }
        public int Column { get; set; }
    }

    public class Scope
    {
        public Dictionary<string, Symbol> Symbols { get; set; } = new Dictionary<string, Symbol>();
        public Scope Parent { get; set; }

        public Symbol Lookup(string name)
        {
            if (Symbols.ContainsKey(name))
                return Symbols[name];

            return Parent?.Lookup(name);
        }

        public void Define(Symbol symbol)
        {
            if (Symbols.ContainsKey(symbol.Name))
                throw new Exception($"Symbol '{symbol.Name}' already defined in this scope");

            Symbols[symbol.Name] = symbol;
        }
    }

    public class SemanticAnalyzer
    {
        private Scope currentScope;
        private List<string> errors;
        private Dictionary<string, string> builtinTypes;

        public SemanticAnalyzer()
        {
            builtinTypes = new Dictionary<string, string>
            {
                {"int", "int"}, {"float", "float"}, {"bool", "bool"},
                {"char", "char"}, {"string", "string"}, {"null", "null"},
                {"capsule", "capsule"}, {"stack", "stack"}, {"heap", "heap"},
                {"mutex", "mutex"}, {"thread", "thread"}, {"core", "core"}
            };
        }

        public SemanticResult Analyze(ProgramNode program)
        {
            errors = new List<string>();
            currentScope = new Scope(); // Global scope

            // Add built-in types to global scope
            foreach (var type in builtinTypes)
            {
                currentScope.Define(new Symbol
                {
                    Name = type.Key,
                    Type = "type",
                    IsType = true
                });
            }

            try
            {
                VisitProgram(program);
            }
            catch (Exception ex)
            {
                errors.Add($"Internal error: {ex.Message}");
            }

            return new SemanticResult
            {
                Success = errors.Count == 0,
                Errors = errors
            };
        }

        private void VisitProgram(ProgramNode program)
        {
            foreach (var declaration in program.Declarations)
            {
                VisitDeclaration(declaration);
            }
        }

        private void VisitDeclaration(DeclarationNode declaration)
        {
            switch (declaration)
            {
                case CapsuleDeclarationNode capsule:
                    VisitCapsuleDeclaration(capsule);
                    break;
                case FunctionDeclarationNode function:
                    VisitFunctionDeclaration(function);
                    break;
                case VariableDeclarationNode variable:
                    VisitVariableDeclaration(variable);
                    break;
                case NamespaceDeclarationNode ns:
                    VisitNamespaceDeclaration(ns);
                    break;
                case StructDeclarationNode structDecl:
                    VisitStructDeclaration(structDecl);
                    break;
                case EnumDeclarationNode enumDecl:
                    VisitEnumDeclaration(enumDecl);
                    break;
                default:
                    errors.Add($"Unknown declaration type: {declaration.GetType()}");
                    break;
            }
        }

        private void VisitCapsuleDeclaration(CapsuleDeclarationNode capsule)
        {
            // Define the capsule type
            var symbol = new Symbol
            {
                Name = capsule.Name,
                Type = "capsule",
                IsType = true,
                Line = capsule.Line,
                Column = capsule.Column
            };

            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {capsule.Line}: {ex.Message}");
            }

            // Enter capsule scope
            var capsuleScope = new Scope { Parent = currentScope };
            currentScope = capsuleScope;

            foreach (var member in capsule.Members)
            {
                VisitDeclaration(member);
            }

            // Exit capsule scope
            currentScope = currentScope.Parent;
        }

        private void VisitFunctionDeclaration(FunctionDeclarationNode function)
        {
            // Check return type
            string returnType = GetTypeName(function.ReturnType);
            if (!IsValidType(returnType))
            {
                errors.Add($"Line {function.Line}: Unknown return type '{returnType}'");
            }

            // Define the function
            var symbol = new Symbol
            {
                Name = function.Name,
                Type = returnType,
                IsFunction = true,
                Line = function.Line,
                Column = function.Column
            };

            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {function.Line}: {ex.Message}");
            }

            // Enter function scope
            var functionScope = new Scope { Parent = currentScope };
            currentScope = functionScope;

            // Check parameters
            foreach (var param in function.Parameters)
            {
                string paramType = GetTypeName(param.Type);
                if (!IsValidType(paramType))
                {
                    errors.Add($"Line {param.Line}: Unknown parameter type '{paramType}'");
                }

                var paramSymbol = new Symbol
                {
                    Name = param.Name,
                    Type = paramType,
                    IsVariable = true,
                    Line = param.Line,
                    Column = param.Column
                };

                try
                {
                    currentScope.Define(paramSymbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {param.Line}: {ex.Message}");
                }
            }

            // Visit function body
            if (function.Body != null)
            {
                VisitStatement(function.Body);
            }

            // Exit function scope
            currentScope = currentScope.Parent;
        }

        private void VisitVariableDeclaration(VariableDeclarationNode variable)
        {
            string varType = GetTypeName(variable.Type);
            if (!IsValidType(varType))
            {
                errors.Add($"Line {variable.Line}: Unknown type '{varType}'");
            }

            var symbol = new Symbol
            {
                Name = variable.Name,
                Type = varType,
                IsVariable = true,
                Line = variable.Line,
                Column = variable.Column
            };

            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {variable.Line}: {ex.Message}");
            }

            if (variable.Initializer != null)
            {
                string initType = VisitExpression(variable.Initializer);
                if (!IsAssignableType(varType, initType))
                {
                    errors.Add($"Line {variable.Line}: Cannot assign {initType} to {varType}");
                }
            }
        }

        private void VisitNamespaceDeclaration(NamespaceDeclarationNode ns)
        {
            // Enter namespace scope
            var namespaceScope = new Scope { Parent = currentScope };
            currentScope = namespaceScope;

            foreach (var member in ns.Members)
            {
                VisitDeclaration(member);
            }

            // Exit namespace scope
            currentScope = currentScope.Parent;
        }

        private void VisitStructDeclaration(StructDeclarationNode structDecl)
        {
            var symbol = new Symbol
            {
                Name = structDecl.Name,
                Type = "struct",
                IsType = true,
                Line = structDecl.Line,
                Column = structDecl.Column
            };

            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {structDecl.Line}: {ex.Message}");
            }

            // Enter struct scope
            var structScope = new Scope { Parent = currentScope };
            currentScope = structScope;

            foreach (var field in structDecl.Fields)
            {
                VisitVariableDeclaration(field);
            }

            // Exit struct scope
            currentScope = currentScope.Parent;
        }

        private void VisitEnumDeclaration(EnumDeclarationNode enumDecl)
        {
            var symbol = new Symbol
            {
                Name = enumDecl.Name,
                Type = "enum",
                IsType = true,
                Line = enumDecl.Line,
                Column = enumDecl.Column
            };

            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {enumDecl.Line}: {ex.Message}");
            }
        }

        private void VisitStatement(StatementNode statement)
        {
            switch (statement)
            {
                case BlockStatementNode block:
                    VisitBlockStatement(block);
                    break;
                case ExpressionStatementNode expr:
                    VisitExpression(expr.Expression);
                    break;
                case IfStatementNode ifStmt:
                    VisitIfStatement(ifStmt);
                    break;
                case LoopStatementNode loop:
                    VisitLoopStatement(loop);
                    break;
                case ReturnStatementNode ret:
                    VisitReturnStatement(ret);
                    break;
                case TryStatementNode tryStmt:
                    VisitTryStatement(tryStmt);
                    break;
                case ThrowStatementNode throwStmt:
                    VisitThrowStatement(throwStmt);
                    break;
                    // Break and Continue don't need semantic checking
            }
        }

        private void VisitBlockStatement(BlockStatementNode block)
        {
            var blockScope = new Scope { Parent = currentScope };
            currentScope = blockScope;

            foreach (var statement in block.Statements)
            {
                VisitStatement(statement);
            }

            currentScope = currentScope.Parent;
        }

        private void VisitIfStatement(IfStatementNode ifStmt)
        {
            string condType = VisitExpression(ifStmt.Condition);
            if (condType != "bool")
            {
                errors.Add($"Line {ifStmt.Line}: If condition must be boolean, got {condType}");
            }

            VisitStatement(ifStmt.ThenStatement);
            if (ifStmt.ElseStatement != null)
            {
                VisitStatement(ifStmt.ElseStatement);
            }
        }

        private void VisitLoopStatement(LoopStatementNode loop)
        {
            string condType = VisitExpression(loop.Condition);
            if (condType != "bool")
            {
                errors.Add($"Line {loop.Line}: Loop condition must be boolean, got {condType}");
            }

            VisitStatement(loop.Body);
        }

        private void VisitReturnStatement(ReturnStatementNode ret)
        {
            if (ret.Expression != null)
            {
                VisitExpression(ret.Expression);
            }
        }

        private void VisitTryStatement(TryStatementNode tryStmt)
        {
            VisitStatement(tryStmt.TryBlock);

            foreach (var catchClause in tryStmt.CatchClauses)
            {
                string exceptionType = GetTypeName(catchClause.ExceptionType);
                if (!IsValidType(exceptionType))
                {
                    errors.Add($"Line {catchClause.Line}: Unknown exception type '{exceptionType}'");
                }

                var catchScope = new Scope { Parent = currentScope };
                currentScope = catchScope;

                var exceptionSymbol = new Symbol
                {
                    Name = catchClause.VariableName,
                    Type = exceptionType,
                    IsVariable = true,
                    Line = catchClause.Line,
                    Column = catchClause.Column
                };

                currentScope.Define(exceptionSymbol);
                VisitStatement(catchClause.Body);
                currentScope = currentScope.Parent;
            }
        }

        private void VisitThrowStatement(ThrowStatementNode throwStmt)
        {
            if (throwStmt.Expression != null)
            {
                VisitExpression(throwStmt.Expression);
            }
        }

        private string VisitExpression(ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralExpressionNode literal:
                    return GetLiteralType(literal.Type);

                case IdentifierExpressionNode identifier:
                    var symbol = currentScope.Lookup(identifier.Name);
                    if (symbol == null)
                    {
                        errors.Add($"Line {identifier.Line}: Undefined identifier '{identifier.Name}'");
                        return "error";
                    }
                    return symbol.Type;

                case BinaryExpressionNode binary:
                    return VisitBinaryExpression(binary);

                case UnaryExpressionNode unary:
                    return VisitUnaryExpression(unary);

                case FunctionCallExpressionNode call:
                    return VisitFunctionCall(call);

                case MemberAccessExpressionNode member:
                    return VisitMemberAccess(member);

                case AssignmentExpressionNode assignment:
                    return VisitAssignment(assignment);

                case NewExpressionNode newExpr:
                    return VisitNewExpression(newExpr);

                default:
                    errors.Add($"Line {expression.Line}: Unknown expression type");
                    return "error";
            }
        }

        private string VisitBinaryExpression(BinaryExpressionNode binary)
        {
            string leftType = VisitExpression(binary.Left);
            string rightType = VisitExpression(binary.Right);

            switch (binary.Operator)
            {
                case TokenType.Plus:
                case TokenType.Minus:
                case TokenType.Multiply:
                case TokenType.Divide:
                    if (leftType == "int" && rightType == "int")
                        return "int";
                    if (leftType == "float" || rightType == "float")
                        return "float";
                    errors.Add($"Line {binary.Line}: Invalid types for binary operation '{binary.Operator}' ({leftType}, {rightType})");
                    return "error";
                case TokenType.Equal:
                case TokenType.NotEqual:
                    if (leftType == rightType || leftType == "null" || rightType == "null")
                        return "bool";
                    errors.Add($"Line {binary.Line}: Cannot compare types '{leftType}' and '{rightType}'");
                    return "error";
                case TokenType.Greater:
                case TokenType.GreaterEqual:
                case TokenType.Less:
                case TokenType.LessEqual:
                    if (leftType == "int" && rightType == "int")
                        return "bool";
                    if (leftType == "float" || rightType == "float")
                        return "bool";
                    errors.Add($"Line {binary.Line}: Invalid comparison between '{leftType}' and '{right
                            Type}'");
                    return "error";
                case TokenType.And:
                case TokenType.Or:
                    if (leftType == "bool" && rightType == "bool")
                        return "bool";
                    errors.Add($"Line {binary.Line}: Logical operation '{binary.Operator}' requires boolean operands");
                    return "error";
                default:
                    errors.Add($"Line {binary.Line}: Unknown binary operator '{binary.Operator}'");
                    return "error";
            }
            private string VisitUnaryExpression(UnaryExpressionNode unary)
        {
            string operandType = VisitExpression(unary.Operand);
            switch (unary.Operator)
            {
                case TokenType.Not:
                    if (operandType == "bool")
                        return "bool";
                    errors.Add($"Line {unary.Line}: '!' operator requires boolean operand, got '{operandType}'");
                    return "error";
                case TokenType.Minus:
                    if (operandType == "int" || operandType == "float")
                        return operandType;
                    errors.Add($"Line {unary.Line}: '-' operator requires numeric operand, got '{operandType}'");
                    return "error";
                default:
                    errors.Add($"Line {unary.Line}: Unknown unary operator '{unary.Operator}'");
                    return "error";
            }
        }
        private string VisitFunctionCall(FunctionCallExpressionNode call)
        {
            var functionSymbol = currentScope.Lookup(call.Function.Name);
            if (functionSymbol == null || !functionSymbol.IsFunction)
            {
                errors.Add($"Line {call.Line}: Undefined function '{call.Function.Name}'");
                return "error";
            }
            // Check argument types
            if (call.Arguments.Count != functionSymbol.Parameters.Count)
            {
                errors.Add($"Line {call.Line}: Function '{call.Function.Name}' expects {functionSymbol.Parameters.Count} arguments, got {call.Arguments.Count}");
                return "error";
            }
            for (int i = 0; i < call.Arguments.Count; i++)
            {
                string argType = VisitExpression(call.Arguments[i]);
                string paramType = functionSymbol.Parameters[i].Type;
                if (!IsAssignableType(paramType, argType))
                {
                    errors.Add($"Line {call.Line}: Cannot assign argument of type '{argType}' to parameter of type '{paramType}'");
                    return "error";
                }
            }
            return functionSymbol.Type; // Return type of the function
        }
        private string VisitMemberAccess(MemberAccessExpressionNode member)
        {
            string objectType = VisitExpression(member.Object);
            if (objectType == "error")
            {
                return "error";
            }
            // Check if the member exists in the object type
            var objectSymbol = currentScope.Lookup(objectType);
            if (objectSymbol == null || !objectSymbol.IsType)
            {
                errors.Add($"Line {member.Line}: Undefined type '{objectType}' for member access");
                return "error";
            }
            // Assuming we have a way to get members of a type
            var memberSymbol = objectSymbol.GetMember(member.MemberName);
            if (memberSymbol == null)
            {
                errors.Add($"Line {member.Line}: Type '{objectType}' has no member '{member.MemberName}'");
                return "error";
            }
            return memberSymbol.Type; // Return the type of the member
        }
        private string VisitAssignment(AssignmentExpressionNode assignment)
        {
            string leftType = VisitExpression(assignment.Left);
            string rightType = VisitExpression(assignment.Right);
            if (leftType == "error" || rightType == "error")
            {
                return "error";
            }
            if (!IsAssignableType(leftType, rightType))
            {
                errors.Add($"Line {assignment.Line}: Cannot assign '{rightType}' to '{leftType}'");
                return "error";
            }
            return leftType; // Return type of the left side
        }
        private string VisitNewExpression(NewExpressionNode newExpr)
        {
            string typeName = GetTypeName(newExpr.Type);
            if (!IsValidType(typeName))
            {
                errors.Add($"Line {newExpr.Line}: Unknown type '{typeName}'");
                return "error";
            }
            // Check constructor arguments
            foreach (var arg in newExpr.Arguments)
            {
                string argType = VisitExpression(arg);
                if (!IsAssignableType(typeName, argType))
                {
                    errors.Add($"Line {newExpr.Line}: Cannot assign argument of type '{argType}' to constructor of type '{typeName}'");
                    return "error";
                }
            }
            return typeName; // Return the type being instantiated
        }
        private string GetTypeName(TypeNode type)
        {
            if (type is IdentifierTypeNode identifierType)
            {
                return identifierType.Name;
            }
            else if (type is PrimitiveTypeNode primitiveType)
            {
                return primitiveType.Name.ToLower();
            }
            // Handle other type nodes as needed
            return "unknown";
        }
        private bool IsValidType(string typeName)
        {
            return builtinTypes.ContainsKey(typeName) || currentScope.Lookup(typeName) != null;
        }
        private bool IsAssignableType(string targetType, string sourceType)
        {
            // Allow assignment if types match or source is null
            if (targetType == sourceType || sourceType == "null")
            {
                return true;
            }
            // Allow assignment between compatible numeric types
            if ((targetType == "int" && sourceType == "float") ||
                (targetType == "float" && sourceType == "int") ||
                (targetType == "int" && sourceType == "int") ||
                (targetType == "float" && sourceType == "float"))
            {
                return true;
            }
            // Allow assignment to capsule, struct, or enum types if source is compatible
            var targetSymbol = currentScope.Lookup(targetType);
            var sourceSymbol = currentScope.Lookup(sourceType);
            if (targetSymbol != null && sourceSymbol != null &&
                (targetSymbol.IsType || targetSymbol.IsFunction) &&
                (sourceSymbol.IsType || sourceSymbol.IsFunction))
            {
                return true;
            }
            // Otherwise, assignment is not allowed
            return false;
        }
        private string GetLiteralType(TokenType type)
        {
            switch (type)
            {
                case TokenType.Integer: return "int";
                case TokenType.Float: return "float";
                case TokenType.Boolean: return "bool";
                case TokenType.Character: return "char";
                case TokenType.String: return "string";
                case TokenType.Null: return "null";
                default: return "unknown";
            }
        }
    }
    using System.Collections.Generic;
    using System.Linq;
    namespace PhoenixCompiler
    {
        public class Parser
        {
            private List<Token> tokens;
            private int position;
            public Parser(List<Token> tokens)
            {
                this.tokens = tokens;
                this.position = 0;
            }
            public ExpressionNode ParseExpression()
            {
                return ParseAssignment();
            }
            private ExpressionNode ParseAssignment()
            {
                var expr = ParseBinary();
                if (Match(TokenType.Equal))
                {
                    var assignment = new AssignmentExpressionNode();
                    SetLocation(assignment);
                    assignment.Left = expr;
                    assignment.Right = ParseAssignment();
                    return assignment;
                }
                return expr;
            }
            private ExpressionNode ParseBinary()
            {
                var expr = ParseUnary();
                while (true)
                {
                    if (Match(TokenType.Plus, TokenType.Minus, TokenType.Multiply, TokenType.Divide,
                              TokenType.Equal, TokenType.NotEqual, TokenType.Greater, TokenType.GreaterEqual,
                              TokenType.Less, TokenType.LessEqual, TokenType.And, TokenType.Or))
                    {
                        var binary = new BinaryExpressionNode();
                        SetLocation(binary);
                        binary.Left = expr;
                        binary.Operator = Previous().Type;
                        binary.Right = ParseUnary();
                        expr = binary;
                    }
                    else
                    {
                        break;
                    }
                }
                return expr;
            }
            private ExpressionNode ParseUnary()
            {
                if (Match(TokenType.Not, TokenType.Minus))
                {
                    var unary = new UnaryExpressionNode();
                    SetLocation(unary);
                    unary.Operator = Previous().Type;
                    unary.Operand = ParseUnary();
                    return unary;
                }
                return ParsePrimary();
            }
            private ExpressionNode ParsePrimary()
            {
                if (Match(TokenType.Null))
                {
                    var literal = new LiteralExpressionNode();
                    SetLocation(literal);
                    literal.Type = TokenType.Null;
                    literal.Value = null;
                    return literal;
                }
                if (Match(TokenType.Boolean))
                {
                    var literal = new LiteralExpressionNode();
                    SetLocation(literal);
                    literal.Type = TokenType.Boolean;
                    literal.Value = bool.Parse(Previous().Value);
                    return literal;
                }
                if (Match(TokenType.Integer))
                {
                    var literal = new LiteralExpressionNode();
                    SetLocation(literal);
                    literal.Type = TokenType.Integer;
                    literal.Value = int.Parse(Previous().Value);
                    return literal;
                }
                if (Match(TokenType.Float))
                {
                    var literal = new LiteralExpressionNode();
                    SetLocation(literal);
                    literal.Type = TokenType.Float;
                    literal.Value = float.Parse(Previous().Value);
                    return literal;
                }
                if (Match(TokenType.Character))
                {
                    var literal = new LiteralExpressionNode();
                    SetLocation(literal);
                    literal.Type = TokenType.Character;
                    literal.Value = Previous().Value[0]; // Assuming single character
                    return literal;
                }
                if (Match(TokenType.String))
                {
                    var literal = new LiteralExpressionNode();
                    SetLocation(literal);
                    literal.Type = TokenType.String;
                    literal.Value = Previous().Value; // String value
                    return literal;
                }
                if (Match(TokenType.Identifier))
                {
                    var identifier = new IdentifierExpressionNode();
                    SetLocation(identifier);
                    identifier.Name = Previous().Value;
                    return identifier;
                }
                if (Match(TokenType.LeftParen))
                {
                    var expr = ParseExpression();
                    Consume(TokenType.RightParen, "Expected ')' after expression");
                    return expr;
                }
                throw new Exception($"Unexpected token {Peek().Type} at line {Peek().Line}");
            }
            private bool Match(params TokenType[] types)
            {
                foreach (var type in types)
                {
                    if (Check(type))
                    {
                        Advance();
                        return true;
                    }
                }
                return false;
            }
            private bool Check(TokenType type)
            {
                if (position >= tokens.Count) return false;
                return tokens[position].Type == type;
            }
            private Token Advance()
            {
                if (position < tokens.Count) position++;
                return Previous();
            }
            private Token Previous()
            {
                if (position == 0) return null;
                return tokens[position - 1];
            }
            private Token Peek()
            {
                if (position >= tokens.Count) return null;
                return tokens[position];
            }
            private void Consume(TokenType type, string errorMessage)
            {
                if (Check(type))
                {
                    Advance();
                }
                else
                {
                    throw new Exception($"{errorMessage} at line {Peek().Line}");
                }
            }
            private void SetLocation(Node node)
            {
                if (Peek() != null)
                {
                    node.Line = Peek().Line;
                    node.Column = Peek().Column;
                }
                else
                {
                    node.Line = 0;
                    node.Column = 0;
                }
            }
            public ProgramNode ParseProgram()
            {
                var program = new ProgramNode();
                SetLocation(program);
                while (Peek() != null && Peek().Type != TokenType.EndOfFile)
                {
                    var declaration = ParseDeclaration();
                    if (declaration != null)
                    {
                        program.Declarations.Add(declaration);
                    }
                }
                Consume(TokenType.EndOfFile, "Expected end of file");
                return program;
            }
            using System;
        using System.Collections.Generic;
        using PhoenixCompiler.AST;
        using PhoenixCompiler.Semantics;
        public class SemanticAnalyzer
        {
            private List<string> errors;
            private Scope currentScope;
            private Dictionary<string, string> builtinTypes;
            public SemanticAnalyzer()
            {
                errors = new List<string>();
                currentScope = new Scope();
                builtinTypes = new Dictionary<string, string>
                    {
                    { "int", "int" },
                    { "float", "float" },
                    { "bool", "bool" },
                    { "char", "char" },
                    { "string", "string" },
                    { "void", "void" },
                    { "null", "null" }
                };
            }
            public SemanticResult Analyze(ProgramNode program)
            {
                errors.Clear();
                currentScope = new Scope();
                // Define built-in types
                foreach (var type in builtinTypes.Keys)
                {
                    currentScope.Define(new Symbol
                    {
                        Name = type,
                        Type = type,
                        IsBuiltin = true,
                        IsType = true,
                        Line = 0,
                        Column = 0
                    });
                }
                // Visit all declarations in the program
                foreach (var declaration in program.Declarations)
                {
                    VisitDeclaration(declaration);
                }
                return new SemanticResult
                {
                    Errors = errors,
                    Scope = currentScope
                };
            }
            private void VisitDeclaration(DeclarationNode declaration)
            {
                switch (declaration)
                {
                    case CapsuleDeclarationNode capsule:
                        VisitCapsuleDeclaration(capsule);
                        break;
                    case FunctionDeclarationNode function:
                        VisitFunctionDeclaration(function);
                        break;
                    case VariableDeclarationNode variable:
                        VisitVariableDeclaration(variable);
                        break;
                    case NamespaceDeclarationNode ns:
                        VisitNamespaceDeclaration(ns);
                        break;
                    case StructDeclarationNode structDecl:
                        VisitStructDeclaration(structDecl);
                        break;
                    case EnumDeclarationNode enumDecl:
                        VisitEnumDeclaration(enumDecl);
                        break;
                    default:
                        errors.Add($"Line {declaration.Line}: Unknown declaration type '{declaration.GetType().
                            Name}'");
                        break;
                }
            }
            private void VisitCapsuleDeclaration(CapsuleDeclarationNode capsule)
            {
                var symbol = new Symbol
                {
                    Name = capsule.Name,
                    Type = "capsule",
                    IsType = true,
                    Line = capsule.Line,
                    Column = capsule.Column
                };
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {capsule.Line}: {ex.Message}");
                }
                // Enter capsule scope
                var capsuleScope = new Scope { Parent = currentScope };
                currentScope = capsuleScope;
                // Visit capsule members
                foreach (var member in capsule.Members)
                {
                    VisitDeclaration(member);
                }
                // Exit capsule scope
                currentScope = currentScope.Parent;
            }
            private void VisitFunctionDeclaration(FunctionDeclarationNode function)
            {
                var symbol = new Symbol
                {
                    Name = function.Name,
                    Type = GetTypeName(function.ReturnType),
                    IsFunction = true,
                    Line = function.Line,
                    Column = function.Column
                };
                if (!IsValidType(symbol.Type))
                {
                    errors.Add($"Line {function.Line}: Unknown return type '{symbol.Type}'");
                }
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {function.Line}: {ex.Message}");
                }
                // Enter function scope
                var functionScope = new Scope { Parent = currentScope };
                currentScope = functionScope;
                // Define function parameters
                foreach (var param in function.Parameters)
                {
                    string paramType = GetTypeName(param.Type);
                    if (!IsValidType(paramType))
                    {
                        errors.Add($"Line {param.Line}: Unknown parameter type '{paramType}'");
                        continue;
                    }
                    var paramSymbol = new Symbol
                    {
                        Name = param.Name,
                        Type = paramType,
                        IsVariable = true,
                        Line = param.Line,
                        Column = param.Column
                    };
                    try
                    {
                        currentScope.Define(paramSymbol);
                    }
                    catch (Exception ex)
                    {
                        errors.Add($"Line {param.Line}: {ex.Message}");
                    }
                }
                // Visit function body
                if (function.Body != null)
                {
                    VisitBlockStatement(function.Body);
                }
                // Exit function scope
                currentScope = currentScope.Parent;
            }
            private void VisitVariableDeclaration(VariableDeclarationNode variable)
            {
                string varType = GetTypeName(variable.Type);
                if (!IsValidType(varType))
                {
                    errors.Add($"Line {variable.Line}: Unknown variable type '{varType}'");
                    return;
                }
                var symbol = new Symbol
                {
                    Name = variable.Name,
                    Type = varType,
                    IsVariable = true,
                    Line = variable.Line,
                    Column = variable.Column
                };
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {variable.Line}: {ex.Message}");
                }
                // Check if variable has an initializer
                if (variable.Initializer != null)
                {
                    string initType = VisitExpression(variable.Initializer);
                    if (!IsAssignableType(varType, initType))
                    {
                        errors.Add($"Line {variable.Line}: Cannot assign '{initType}' to variable of type '{varType}'");
                    }
                }
            }
            private void VisitNamespaceDeclaration(NamespaceDeclarationNode ns)
            {
                // Create a new scope for the namespace
                var namespaceScope = new Scope { Parent = currentScope };
                currentScope = namespaceScope;
                // Define the namespace symbol
                var symbol = new Symbol
                {
                    Name = ns.Name,
                    Type = "namespace",
                    IsType = true,
                    Line = ns.Line,
                    Column = ns.Column
                };
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {ns.Line}: {ex.Message}");
                }
                // Visit all declarations within the namespace
                foreach (var declaration in ns.Declarations)
                {
                    VisitDeclaration(declaration);
                }
                // Exit the namespace scope
                currentScope = currentScope.Parent;
            }
            private void VisitStructDeclaration(StructDeclarationNode structDecl)
            {
                var symbol = new Symbol
                {
                    Name = structDecl.Name,
                    Type = "struct",
                    IsType = true,
                    Line = structDecl.Line,
                    Column = structDecl.Column
                };
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {structDecl.Line}: {ex.Message}");
                }
                // Enter struct scope
                var structScope = new Scope { Parent = currentScope };
                currentScope = structScope;
                // Visit struct members
                foreach (var member in structDecl.Members)
                {
                    VisitDeclaration(member);
                }
                // Exit struct scope
                currentScope = currentScope.Parent;
            }
            private void VisitEnumDeclaration(EnumDeclarationNode enumDecl)
            {
                var symbol = new Symbol
                {
                    Name = enumDecl.Name,
                    Type = "enum",
                    IsType = true,
                    Line = enumDecl.Line,
                    Column = enumDecl.Column
                };
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {enumDecl.Line}: {ex.Message}");
                }
                // Enter enum scope
                var enumScope = new Scope { Parent = currentScope };
                currentScope = enumScope;
                // Visit enum members
                foreach (var member in enumDecl.Members)
                {
                    var memberSymbol = new Symbol
                    {
                        Name = member.Name,
                        Type = "enum_member",
                        IsVariable = true,
                        Line = member.Line,
                        Column = member.Column
                    };
                    try
                    {
                        currentScope.Define(memberSymbol);
                    }
                    catch (Exception ex)
                    {
                        errors.Add($"Line {member.Line}: {ex.Message}");
                    }
                }
                // Exit enum scope
                currentScope = currentScope.Parent;
            }
            private void VisitBlockStatement(BlockStatementNode block)
            {
                // Create a new scope for the block
                var blockScope = new Scope { Parent = currentScope };
                currentScope = blockScope;
                // Visit all statements in the block
                foreach (var statement in block.Statements)
                {
                    VisitStatement(statement);
                }
                // Exit the block scope
                currentScope = currentScope.Parent;
            }
            private void VisitStatement(StatementNode statement)
            {
                switch (statement)
                {
                    case ExpressionStatementNode exprStmt:
                        VisitExpression(exprStmt.Expression);
                        break;
                    case VariableDeclarationStatementNode varDeclStmt:
                        VisitVariableDeclaration(varDeclStmt.Variable);
                        break;
                    case IfStatementNode ifStmt:
                        VisitIfStatement(ifStmt);
                        break;
                    case WhileStatementNode whileStmt:
                        VisitWhileStatement(whileStmt);
                        break;
                    case ForStatementNode forStmt:
                        VisitForStatement(forStmt);
                        break;
                    case ReturnStatementNode returnStmt:
                        VisitReturnStatement(returnStmt);
                        break;
                    case ThrowStatementNode throwStmt:
                        VisitThrowStatement(throwStmt);
                        break;
                    default:
                        errors.Add($"Line {statement.Line}: Unknown statement type '{statement.GetType().Name}'");
                        break;
                }
            }
            private void VisitIfStatement(IfStatementNode ifStmt)
            {
                string conditionType = VisitExpression(ifStmt.Condition);
                if (conditionType != "bool")
                {
                    errors.Add($"Line {ifStmt.Line}: 'if' condition must be boolean, got '{conditionType}'");
                }
                VisitBlockStatement(ifStmt.ThenBlock);
                if (ifStmt.ElseBlock != null)
                {
                    VisitBlockStatement(ifStmt.ElseBlock);
                }
            }
            private void VisitWhileStatement(WhileStatementNode whileStmt)
            {
                string conditionType = VisitExpression(whileStmt.Condition);
                if (conditionType != "bool")
                {
                    errors.Add($"Line {whileStmt.Line}: 'while' condition must be boolean, got '{conditionType}'");
                }
                VisitBlockStatement(whileStmt.Body);
            }
            private void VisitForStatement(ForStatementNode forStmt)
            {
                // Visit initialization
                if (forStmt.Initialization != null)
                {
                    VisitStatement(forStmt.Initialization);
                }
                // Visit condition
                if (forStmt.Condition != null)
                {
                    string conditionType = VisitExpression(forStmt.Condition);
                    if (conditionType != "bool")
                    {
                        errors.Add($"Line {forStmt.Line}: 'for' condition must be boolean, got '{conditionType}'");
                    }
                }
                // Visit increment
                if (forStmt.Increment != null)
                {
                    VisitExpression(forStmt.Increment);
                }
                // Visit body
                VisitBlockStatement(forStmt.Body);
            }
            private void VisitReturnStatement(ReturnStatementNode returnStmt)
            {
                if (returnStmt.Expression != null)
                {
                    string returnType = VisitExpression(returnStmt.Expression);
                    // Check if the function has a return type
                    var functionSymbol = currentScope.GetFunctionSymbol();
                    if (functionSymbol != null && functionSymbol.Type != "void")
                    {
                        if (!IsAssignableType(functionSymbol.Type, returnType))
                        {
                            errors.Add($"Line {returnStmt.Line}: Cannot return '{returnType}' from function returning '{functionSymbol.Type}'");
                        }
                    }
                }
                else
                {
                    // If the function has a return type, it must not be void
                    var functionSymbol = currentScope.GetFunctionSymbol();
                    if (functionSymbol != null && functionSymbol.Type != "void")
                    {
                        errors.Add($"Line {returnStmt.Line}: Cannot return from function '{functionSymbol.Name}' without a value");
                    }
                }
            }
            private void VisitThrowStatement(ThrowStatementNode throwStmt)
            {
                if (throwStmt.Expression != null)
                {
                    string throwType = VisitExpression(throwStmt.Expression);
                    // Check if the thrown type is valid
                    if (!IsValidType(throwType))
                    {
                        errors.Add($"Line {throwStmt.Line}: Cannot throw '{throwType}', it is not a valid type");
                    }
                }
                else
                {
                    errors.Add($"Line {throwStmt.Line}: 'throw' statement must have an expression");
                }
            }
            private string VisitExpression(ExpressionNode expr)
            {
                switch (expr)
                {
                    case LiteralExpressionNode literal:
                        return GetLiteralType(literal.Type);
                    case IdentifierExpressionNode identifier:
                        var symbol = currentScope.Lookup(identifier.Name);
                        if (symbol == null)
                        {
                            errors.Add($"Line {identifier.Line}: Undefined identifier '{identifier.Name}'");
                            return "error";
                        }
                        return symbol.Type; // Return the type of the identifier
                    case BinaryExpressionNode binary:
                        string leftType = VisitExpression(binary.Left);
                        string rightType = VisitExpression(binary.Right);
                        if (leftType == "error" || rightType == "error")
                        {
                            return "error";
                        }
                        // Check if the binary operation is valid for the types
                        if (!IsValidBinaryOperation(leftType, rightType, binary.Operator))
                        {
                            errors.Add($"Line {binary.Line}: Invalid operation '{binary.Operator}' for types '{leftType}' and '{rightType}'");
                            return "error";
                        }
                        // Return the result type based on the operation
                        return GetResultType(leftType, rightType, binary.Operator);
                    case UnaryExpressionNode unary:
                        string operandType = VisitExpression(unary.Operand);
                        if (operandType == "error")
                        {
                            return "error";
                        }
                        // Check if the unary operation is valid for the type
                        if (!IsValidUnaryOperation(operandType, unary.Operator))
                        {
                            errors.Add($"Line {unary.Line}: Invalid operation '{unary.Operator}' for type '{operandType}'");
                            return "error";
                        }
                        // Return the result type based on the unary operation
                        return GetUnaryResultType(operandType, unary.Operator);
                    case AssignmentExpressionNode assignment:
                        string leftType = VisitExpression(assignment.Left);
                        string rightType = VisitExpression(assignment.Right);
                        if (leftType == "error" || rightType == "error")
                        {
                            return "error";
                        }
                        // Check if the assignment is valid
                        if (!IsAssignableType(leftType, rightType))
                        {
                            errors.Add($"Line {assignment.Line}: Cannot assign '{rightType}' to '{leftType}'");
                            return "error";
                        }
                        return leftType; // Return the type of the left side of the assignment
                    case NewExpressionNode newExpr:
                        return VisitNewExpression(newExpr);
                    case CallExpressionNode call:
                        var functionSymbol = currentScope.Lookup(call.FunctionName);
                        if (functionSymbol == null || !functionSymbol.IsFunction)
                        {
                            errors.Add($"Line {call.Line}: Undefined function '{call.FunctionName}'");
                            return "error";
                        }
                        // Check argument types
                        if (call.Arguments.Count != functionSymbol.Parameters.Count)
                        {
                            errors.Add($"Line {call.Line}: Function '{call.FunctionName}' expects {functionSymbol.Parameters.Count} arguments, got {call.Arguments.Count}");
                            return "error";
                        }
                        for (int i = 0; i < call.Arguments.Count; i++)
                        {
                            string argType = VisitExpression(call.Arguments[i]);
                            if (argType == "error")
                            {
                                return "error";
                            }
                            string paramType = functionSymbol.Parameters[i].Type;
                            if (!IsAssignableType(paramType, argType))
                            {
                                errors.Add($"Line {call.Line}: Cannot assign argument of type '{argType}' to
                                parameter of type '{paramType}' in function '{call.FunctionName}'");
                                return "error";
                            }
                        }
                        // Return the function's return type
                        return functionSymbol.Type;
                    default:
                        errors.Add($"Line {expr.Line}: Unknown expression type '{expr.GetType().Name}'
                            ");
                            return "error";
                }
            }
            private string GetTypeName(TypeNode typeNode)
            {
                if (typeNode is PrimitiveTypeNode primitive)
                {
                    return primitive.Name.ToLower();
                }
                if (typeNode is IdentifierTypeNode identifier)
                {
                    return identifier.Name;
                }
                if (typeNode is ArrayTypeNode array)
                {
                    return $"{GetTypeName(array.ElementType)}[]";
                }
                if (typeNode is CapsuleTypeNode capsule)
                {
                    return $"capsule.{capsule.Name.ToLower()}"; // Assuming capsule types are prefixed with "capsule."
                }
                if (typeNode is StructTypeNode structType)
                {
                    return $"struct.{structType.Name.ToLower()}"; // Assuming struct types are prefixed with "struct."
                }
                if (typeNode is EnumTypeNode enumType)
                {
                    return $"enum.{enumType.Name.ToLower()}"; // Assuming enum types are prefixed
                    return "error";
                }
                if (typeNode is NamespaceTypeNode namespaceType)
                {
                    return $"namespace.{namespaceType.Name.ToLower()}"; // Assuming namespace types are prefixed
                }
                return "error";
            }
            private bool IsValidType(string type)
            {
                return builtinTypes.ContainsKey(type) || type.StartsWith("capsule.") || type.StartsWith("struct.")
                    || type.StartsWith("enum.") || type.StartsWith("namespace.");
            }
            private bool IsAssignableType(string targetType, string sourceType)
            {
                // Check if the source type can be assigned to the target type
                if (targetType == sourceType)
                {
                    return true;
                }
                // Allow implicit conversions for primitive types
                if (builtinTypes.ContainsKey(targetType) && builtinTypes.ContainsKey(sourceType))
                {
                    // Example: int can be assigned to float, but not vice versa
                    if (targetType == "float" && sourceType == "int")
                    {
                        return true;
                    }
                    if (targetType == "int" && sourceType == "float")
                    {
                        return false; // Cannot assign float to int without explicit cast
                    }
                    if (targetType == "bool" && sourceType == "int")
                    {
                        return true; // Allow int to bool conversion
                    }
                    if (targetType == "int" && sourceType == "bool")
                    {
                        return false; // Cannot assign bool to int
                    }
                    if (targetType == "char" && sourceType == "int")
                    {
                        return true; // Allow int to char conversion
                    }
                    if (targetType == "int" && sourceType == "char")
                    {
                        return false; // Cannot assign char to int
                    }
                    if (targetType == "string" && sourceType == "char")
                    {
                        return true; // Allow char to string conversion
                    }
                    if (targetType == "char" && sourceType == "string")
                    {
                        return false; // Cannot assign string to char
                    }
                    // Add more implicit conversion rules as needed
                }
                // Check if the target type is a capsule, struct, enum, or namespace
                if (targetType.StartsWith("capsule.") || targetType.StartsWith("struct.")
                    || targetType.StartsWith("enum.") || targetType.StartsWith("namespace."))
                {
                    // Allow assignment if the source type is the same or a subtype
                    if (sourceType.StartsWith(targetType) || sourceType == targetType)
                    {
                        return true;
                    }
                    // Allow assignment if the source type is a built-in type that can be converted to the target type
                    if (builtinTypes.ContainsKey(sourceType) && builtinTypes.ContainsKey(targetType))
                    {
                        // Example: int can be assigned to capsule.int, but not vice versa
                        if (targetType == "capsule.int" && sourceType == "int")
                        {
                            return true;
                        }
                        if (targetType == "capsule.float" && sourceType == "float")
                        {
                            return true;
                        }
                        if (targetType == "capsule.bool" && sourceType == "bool")
                        {
                            return true;
                        }
                        if (targetType == "capsule.char" && sourceType == "char")
                        {
                            return true;
                        }
                        if (targetType == "capsule.string" && sourceType == "string")
                        {
                            return true;
                        }
                    }
                }
                // If no rules matched, return false
                return false;
            }
            private string GetLiteralType(LiteralType type)
            {
                switch (type)
                {
                    case LiteralType.Int:
                        return "int";
                    case LiteralType.Float:
                        return "float";
                    case LiteralType.Bool:
                        return "bool";
                    case LiteralType.Char:
                        return "char";
                    case LiteralType.String:
                        return "string";
                    case LiteralType.Null:
                        return "null";
                    default:
                        return "error";
                }
            }
            private bool IsValidBinaryOperation(string leftType, string rightType, BinaryOperator op)
            {
                // Check if the binary operation is valid for the given types
                if (op == BinaryOperator.Add || op == BinaryOperator.Subtract || op == Binary
                    Operator.Multiply || op == BinaryOperator.Divide)
                {
                    // Allow arithmetic operations on numeric types
                    return (leftType == "int" && rightType == "int") ||
                           (leftType == "float" && rightType == "float") ||
                           (leftType == "int" && rightType == "float") ||
                           (leftType == "float" && rightType == "int");
                }
                if (op == BinaryOperator.Equal || op == BinaryOperator.NotEqual)
                {
                    // Allow equality checks between any types
                    return true;
                }
                if (op == BinaryOperator.LessThan || op == BinaryOperator.GreaterThan ||
                    op == BinaryOperator.LessThanOrEqual || op == BinaryOperator.GreaterThanOr
                    Equal)
                        {
                    // Allow comparison operations on numeric types and strings
                    return (leftType == "int" && rightType == "int") ||
                            (leftType == "float" && rightType == "float") ||
                            (leftType == "int" && rightType == "float") ||
                            (leftType == "float" && rightType == "int") ||
                            (leftType == "string" && rightType == "string");
                }
                if (op == BinaryOperator.And || op == BinaryOperator.Or)
                {
                    // Allow logical operations on boolean types
                    return (leftType == "bool" && rightType == "bool");
                }
                if (op == BinaryOperator.Assignment)
                {
                    // Assignment is valid if the left side is a variable or a property
                    // and the right side is a compatible type
                    return IsAssignableType(leftType, rightType);
                }
                // If no rules matched, return false
                return false;
            }
            private string GetResultType(string leftType, string rightType, BinaryOperator op)
            {
                // Determine the result type based on the operation
                switch (op)
                {
                    case BinaryOperator.Add:
                    case BinaryOperator.Subtract:
                    case BinaryOperator.Multiply:
                    case BinaryOperator.Divide:
                        // Result type is the more general type
                        if (leftType == "float" || rightType == "float")
                        {
                            return "float";
                        }
                        return "int"; // Both are int
                    case BinaryOperator.Equal:
                    case BinaryOperator.NotEqual:
                        return "bool"; // Equality checks always return bool
                    case BinaryOperator.LessThan:
                    case BinaryOperator.GreaterThan:
                    case BinaryOperator.LessThanOrEqual:
                    case BinaryOperator.GreaterThanOrEqual:
                        return "bool"; // Comparison operations always return bool
                    case BinaryOperator.And:
                    case BinaryOperator.Or:
                        return "bool"; // Logical operations always return bool
                    default:
                        return "error"; // Unknown operation
                }
            }
            private string GetUnaryResultType(string operandType, UnaryOperator op)
            {
                // Determine the result type based on the unary operation
                switch (op)
                {
                    case UnaryOperator.Negate:
                        if (operandType == "int" || operandType == "float")
                        {
                            return operandType; // Negation returns the same type
                        }
                        errors.Add($"Line {operandType}: Cannot negate type '{operandType}'");
                        return "error";
                    case UnaryOperator.Not:
                        if (operandType == "bool")
                        {
                            return "bool"; // Logical NOT returns bool
                        }
                        errors.Add($"Line {operandType}: Cannot apply 'not' to type '{operandType}'");
                        return "error";
                    default:
                        errors.Add($"Line {operandType}: Unknown unary operator '{op}'");
                        return "error";
                }
            }
            using System;
        using System.Collections.Generic;
        using System.Linq;
        using MyLanguage.AST; // Assuming AST classes are defined in this namespace
        using MyLanguage.Symbols; // Assuming Symbol and Scope classes are defined in this namespace
        using MyLanguage.Errors; // Assuming error handling is defined in this namespace
        public class SemanticAnalyzer
        {
            private Scope currentScope;
            private List<string> errors;
            private readonly Dictionary<string, string> builtinTypes = new Dictionary<string, string>
                {
                    { "int", "int" },
                    { "float", "float" },
                    { "bool", "bool" },
                    { "char", "char" },
                    { "string", "string" }
                };
            public SemanticAnalyzer()
            {
                currentScope = new Scope();
                errors = new List<string>();
            }
            public List<string> Analyze(List<DeclarationNode> declarations)
            {
                foreach (var declaration in declarations)
                {
                    VisitDeclaration(declaration);
                }
                return errors;
            }
            private void VisitDeclaration(DeclarationNode declaration)
            {
                switch (declaration)
                {
                    case CapsuleDeclarationNode capsule:
                        VisitCapsuleDeclaration(capsule);
                        break;
                    case FunctionDeclarationNode function:
                        VisitFunctionDeclaration(function);
                        break;
                    case VariableDeclarationNode variable:
                        VisitVariableDeclaration(variable);
                        break;
                    case NamespaceDeclarationNode ns:
                        VisitNamespaceDeclaration(ns);
                        break;
                    case StructDeclarationNode structDecl:
                        VisitStructDeclaration(structDecl);
                        break;
                    case EnumDeclarationNode enumDecl:
                        VisitEnumDeclaration(enumDecl);
                        break;
                    default:
                        errors.Add($"Line {declaration.Line}: Unknown declaration type '{declaration.GetType().Name}'");
                        break;
                }
            }
            private void VisitCapsuleDeclaration(CapsuleDeclarationNode capsule)
            {
                var symbol = new Symbol
                {
                    Name = capsule.Name,
                    Type = "capsule",
                    IsType = true,
                    Line = capsule.Line,
                    Column = capsule.Column
                };
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {capsule.Line}: {ex.Message}");
                }
                // Enter capsule scope
                var capsuleScope =
                    new Scope { Parent = currentScope };
                currentScope = capsuleScope;
                // Visit all members of the capsule
                foreach (var member in capsule.Members)
                {
                    VisitDeclaration(member);
                }
                // Exit capsule scope
                currentScope = currentScope.Parent;
            }
            private void VisitFunctionDeclaration(FunctionDeclarationNode function)
            {
                // Create a new function symbol
                var symbol = new Symbol
                {
                    Name = function.Name,
                    Type = function.ReturnType != null ? GetTypeName(function.ReturnType) : "void",
                    IsFunction = true,
                    Line = function.Line,
                    Column = function.Column
                };
                // Define the function in the current scope
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {function.Line}: {ex.Message}");
                    return;
                }
                // Create a new scope for the function
                var functionScope = new Scope { Parent = currentScope };
                currentScope = functionScope;
                // Define parameters in the function scope
                foreach (var param in function.Parameters)
                {
                    var paramSymbol = new Symbol
                    {
                        Name = param.Name,
                        Type = GetTypeName(param.Type),
                        IsVariable = true,
                        Line = param.Line,
                        Column = param.Column
                    };
                    try
                    {
                        currentScope.Define(paramSymbol);
                    }
                    catch (Exception ex)
                    {
                        errors.Add($"Line {param.Line}: {ex.Message}");
                    }
                }
                // Visit the function body
                if (function.Body != null)
                {
                    VisitBlockStatement(function.Body);
                }
                // Exit function scope
                currentScope = currentScope.Parent;
            }
            private void VisitVariableDeclaration(VariableDeclarationNode variable)
            {
                // Create a new variable symbol
                var symbol = new Symbol
                {
                    Name = variable.Name,
                    Type = GetTypeName(variable.Type),
                    IsVariable = true,
                    Line = variable.Line,
                    Column = variable.Column
                };
                // Check if the type is valid
                if (!IsValidType(symbol.Type))
                {
                    errors.Add($"Line {variable.Line}: Invalid type '{symbol.Type}' for variable '{symbol.Name}'");
                    return;
                }
                // Define the variable in the current scope
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {variable.Line}: {ex.Message}");
                }
            }
            private void VisitNamespaceDeclaration(NamespaceDeclarationNode ns)
            {
                // Create a new namespace symbol
                var symbol = new Symbol
                {
                    Name = ns.Name,
                    Type = "namespace",
                    IsType = true,
                    Line = ns.Line,
                    Column = ns.Column
                };
                // Define the namespace in the current scope
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {ns.Line}: {ex.Message}");
                    return;
                }
                // Enter namespace scope
                var namespaceScope = new Scope { Parent = currentScope };
                currentScope = namespaceScope;
                // Visit all declarations in the namespace
                foreach (var declaration in ns.Declarations)
                {
                    VisitDeclaration(declaration);
                }
                // Exit namespace scope
                currentScope = currentScope.Parent;
            }
            private void VisitStructDeclaration(StructDeclarationNode structDecl)
            {
                // Create a new struct symbol
                var symbol = new Symbol
                {
                    Name = structDecl.Name,
                    Type = "struct",
                    IsType = true,
                    Line = structDecl.Line,
                    Column = structDecl.Column
                };
                // Define the struct in the current scope
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {structDecl.Line}: {ex.Message}");
                    return;
                }
                // Enter struct scope
                var structScope = new Scope { Parent = currentScope };
                currentScope = structScope;
                // Visit all members of the struct
                foreach (var member in structDecl.Members)
                {
                    VisitDeclaration(member);
                }
                // Exit struct scope
                currentScope = currentScope.Parent;
            }
            private void VisitEnumDeclaration(EnumDeclarationNode enumDecl)
            {
                // Create a new enum symbol
                var symbol = new Symbol
                {
                    Name = enumDecl.Name,
                    Type = "enum",
                    IsType = true,
                    Line = enumDecl.Line,
                    Column = enumDecl.Column
                };
                // Define the enum in the current scope
                try
                {
                    currentScope.Define(symbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {enumDecl.Line}: {ex.Message}");
                    return;
                }
                // Enter enum scope
                var enumScope = new Scope { Parent = currentScope };
                currentScope = enumScope;
                // Visit all members of the enum
                foreach (var member in enumDecl.Members)
                {
                    var memberSymbol = new Symbol
                    {
                        Name = member.Name,
                        Type = "int", // Enum members are typically of type int
                        IsVariable = true,
                        Line = member.Line,
                        Column = member.Column
                    };
                    try
                    {
                        currentScope.Define(memberSymbol);
                    }
                    catch (Exception ex)
                    {
                        errors.Add($"Line {member.Line}: {ex.Message}");
                    }
                }
                // Exit enum scope
                currentScope = currentScope.Parent;
            }
            private void VisitBlockStatement(BlockStatementNode block)
            {
                // Create a new scope for the block
                var blockScope = new Scope { Parent = currentScope };
                currentScope = blockScope;
                // Visit all statements in the block
                foreach (var statement in block.Statements)
                {
                    VisitStatement(statement);
                }
                // Exit block scope
                currentScope = currentScope.Parent;
            }
            private void VisitStatement(StatementNode stmt)
            {
                switch (stmt)
                {
                    case ExpressionStatementNode exprStmt:
                        VisitExpressionStatement(exprStmt);
                        break;
                    case IfStatementNode ifStmt:
                        VisitIfStatement(ifStmt);
                        break;
                    case WhileStatementNode whileStmt:
                        VisitWhileStatement(whileStmt);
                        break;
                    case ForStatementNode forStmt:
                        VisitForStatement(forStmt);
                        break;
                    case ReturnStatementNode returnStmt:
                        VisitReturnStatement(returnStmt);
                        break;
                    case ThrowStatementNode throwStmt:
                        VisitThrowStatement(throwStmt);
                        break;
                    case BlockStatementNode blockStmt:
                        VisitBlockStatement(blockStmt);
                        break;
                    default:
                        errors.Add($"Line {stmt.Line}: Unknown statement type '{stmt.GetType().Name}'");
                        break;
                }
            }
            private void VisitExpressionStatement(ExpressionStatementNode exprStmt)
            {
                // Visit the expression in the statement
                if (exprStmt.Expression != null)
                {
                    string exprType = VisitExpression(exprStmt.Expression);
                    // Check if the expression type is valid
                    if (!IsValidType(exprType))
                    {
                        errors.Add($"Line {exprStmt.Line}: Invalid expression type '{exprType}'");
                    }
                }
                else
                {
                    errors.Add($"Line {exprStmt.Line}: Expression statement must have an expression");
                }
            }
            private void VisitIfStatement(IfStatementNode ifStmt)
            {
                // Visit the condition expression
                string conditionType = VisitExpression(ifStmt.Condition);
                // Check if the condition is a boolean type
                if (conditionType != "bool")
                {
                    errors.Add($"Line {ifStmt.Line}: If condition must be of type 'bool', found '{conditionType}'");
                }
                // Visit the true branch
                if (ifStmt.TrueBranch != null)
                {
                    VisitBlockStatement(ifStmt.TrueBranch);
                }
                // Visit the false branch if it exists
                if (ifStmt.FalseBranch != null)
                {
                    VisitBlockStatement(ifStmt.FalseBranch);
                }
            }
            private void VisitWhileStatement(WhileStatementNode whileStmt)
            {
                // Visit the condition expression
                string conditionType = VisitExpression(whileStmt.Condition);
                // Check if the condition is a boolean type
                if (conditionType != "bool")
                {
                    errors.Add($"Line {whileStmt.Line}: While condition must be of type 'bool', found '{conditionType}'");
                }
                // Visit the body of the while loop
                if (whileStmt.Body != null)
                {
                    VisitBlockStatement(whileStmt.Body);
                }
            }
            private void VisitForStatement(ForStatementNode forStmt)
            {
                // Visit the initialization expression
                if (forStmt.Initialization != null)
                {
                    string initType = VisitExpression(forStmt.Initialization);
                    // Check if the initialization is a valid variable declaration
                    if (!IsValidType(initType))
                    {
                        errors.Add($"Line {forStmt.Line}: Invalid initialization type '{initType}'");
                    }
                }
                // Visit the condition expression
                if (forStmt.Condition != null)
                {
                    string conditionType = VisitExpression(forStmt.Condition);
                    // Check if the condition is a boolean type
                    if (conditionType != "bool")
                    {
                        errors.Add($"Line {forStmt.Line}: For condition must be of type 'bool', found '{conditionType}'");
                    }
                }
                // Visit the increment expression
                if (forStmt.Increment != null)
                {
                    string incrementType = VisitExpression(forStmt.Increment);
                    // Check if the increment is a valid expression
                    if (!IsValidType(incrementType))
                    {
                        errors.Add($"Line {forStmt.Line}: Invalid increment type '{incrementType}'");
                    }
                }
                // Visit the body of the for loop
                if (forStmt.Body != null)
                {
                    VisitBlockStatement(forStmt.Body);
                }
            }
            private void VisitReturnStatement(ReturnStatementNode returnStmt)
            {
                // Check if the return type matches the function's return type
                if (currentScope.FunctionSymbol == null)
                {
                    errors.Add($"Line {returnStmt.Line}: Return statement outside of function");
                    return;
                }
                string returnType = returnStmt.Expression != null ? VisitExpression(returnStmt.Expression) : "void";
                if (returnType == "error")
                {
                    return;
                }
                if (currentScope.FunctionSymbol.Type != "void" && returnType != currentScope.FunctionSymbol.Type)
                {
                    errors.Add($"Line {returnStmt.Line}: Return type '{returnType}' does not match function return type '{currentScope.FunctionSymbol.Type}'");
                }
                else if (currentScope.FunctionSymbol.Type == "void" && returnType != "void")
                {
                    errors.Add($"Line {returnStmt.Line}: Cannot return value '{returnType}' from a void function");
                }
            }
            private void VisitThrowStatement(ThrowStatementNode throwStmt)
            {
                // Check if the throw expression is valid
                if (throwStmt.Expression != null)
                {
                    string throwType = VisitExpression(throwStmt.Expression);
                    // Check if the throw type is a valid exception type
                    if (!IsValidType(throwType) || !throwType.StartsWith("capsule."))
                    {
                        errors.Add($"Line {throwStmt.Line}: Invalid throw type '{throwType}', must be a capsule type");
                    }
                }
                else
                {
                    errors.Add($"Line {throwStmt.Line}: Throw statement must have an expression");
                }
            }
            private string VisitExpression(ExpressionNode expr)
            {
                switch (expr)
                {
                    case LiteralNode literal:
                        return GetLiteralType(literal.Type);
                    case IdentifierNode identifier:
                        return VisitIdentifier(identifier);
                    case BinaryExpressionNode binary:
                        return VisitBinaryExpression(binary);
                    case UnaryExpressionNode unary:
                        return VisitUnaryExpression(unary);
                    case FunctionCallNode functionCall:
                        return VisitFunctionCall(functionCall);
                    case MemberAccessNode memberAccess:
                        return VisitMemberAccess(memberAccess);
                    default:
                        errors.Add($"Line {expr.Line}: Unknown expression type '{expr.GetType().Name}'");
                        return "error";
                }
            }
            private string VisitIdentifier(IdentifierNode identifier)
            {
                // Look up the identifier in the current scope
                var symbol = currentScope.Resolve(identifier.Name);
                if (symbol == null)
                {
                    errors.Add($"Line {identifier.Line}: Identifier '{identifier.Name}' not found");
                    return "error";
                }
                if (symbol.IsVariable || symbol.IsFunction || symbol.IsType)
                {
                    return symbol.Type; // Return the type of the variable or function
                }
                errors.Add($"Line {identifier.Line}: Identifier '{identifier.Name}' is not a variable or function");
                return "error"; // Identifier is not a variable or function
            }
            private string VisitBinaryExpression(BinaryExpressionNode binary)
            {
                // Visit the left and right expressions
                string leftType = VisitExpression(binary.Left);
                string rightType = VisitExpression(binary.Right);
                // Check if the binary operation is valid for the given types
                if (!IsValidBinaryOperation(leftType, rightType, binary.Operator))
                {
                    errors.Add($"Line {binary.Line}: Invalid binary operation '{binary.Operator}' for types '{leftType}' and '{rightType}'");
                    return "error";
                }
                // Get the result type of the binary operation
                string resultType = GetResultType(leftType, rightType, binary.Operator);
                if (resultType == "error")
                {
                    errors.Add($"Line {binary.Line}: Unknown binary operation '{binary.Operator}'");
                    return "error";
                }
                // If the result type is a capsule, struct, enum, or namespace, return its full name
                if (resultType.StartsWith("capsule.") || resultType.StartsWith("struct.")
                    || resultType.StartsWith("enum.") || resultType.StartsWith("namespace."))
                {
                    return resultType; // Return the full name of the type
                }
                // Otherwise, return the primitive type
                return resultType; // Return the primitive type
            }
            private string VisitUnaryExpression(UnaryExpressionNode unary)
            {
                // Visit the operand expression
                string operandType = VisitExpression(unary.Operand);
                // Check if the unary operation is valid for the given type
                if (unary.Operator == UnaryOperator.Negate || unary.Operator == UnaryOperator.Not)
                {
                    string resultType = GetUnaryResultType(operandType, unary.Operator);
                    if (resultType == "error")
                    {
                        errors.Add($"Line {unary.Line}: Invalid unary operation '{unary.Operator}' for type '{operandType}'");
                        return "error";
                    }
                    return resultType; // Return the result type of the unary operation
                }
                errors.Add($"Line {unary.Line}: Unknown unary operator '{unary.Operator}'");
                return "error"; // Unknown unary operator
            }
            private string VisitFunctionCall(FunctionCallNode functionCall)
            {
                // Look up the function in the current scope
                var symbol = currentScope.Resolve(functionCall.Name);
                if (symbol == null || !symbol.IsFunction)
                {
                    errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' not found");
                    return "error";
                }
                // Check if the function has the correct number of arguments
                if (symbol.Parameters.Count != functionCall.Arguments.Count)
                {
                    errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' expects {symbol.Parameters.Count} arguments, but got {functionCall.Arguments.Count}");
                    return "error";
                }
                // Check the types of the arguments
                for (int i = 0; i < functionCall.Arguments.Count; i++)
                {
                    string argType = VisitExpression(functionCall.Arguments[i]);
                    if (!IsAssignableType(argType, symbol.Parameters[i].Type))
                    {
                        errors.Add($"Line {functionCall.Line}: Argument {i + 1} of function '{functionCall.Name}' expects type '{symbol.Parameters[i].Type}', but got '{argType}'");
                        return "error";
                    }
                }
                // If the function has a return type, return it
                if (symbol.Type != "void")
                {
                    return symbol.Type; // Return the function's return type
                }
                // If the function is void, return "void"
                return "void"; // Return void for functions that do not return a value
            }
            private string VisitMemberAccess(MemberAccessNode memberAccess)
            {
                // Visit the object expression
                string objectType = VisitExpression(memberAccess.Object);
                // Check if the object type is valid
                if (objectType == "error")
                {
                    errors.Add($"Line {memberAccess.Line}: Invalid object type for member access");
                    return "error";
                }
                // Look up the member in the current scope or the object's type
                var symbol = currentScope.ResolveMember(objectType, memberAccess.MemberName);
                if (symbol == null)
                {
                    errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in type '{objectType}'");
                    return "error";
                }
                // If the member is a variable or property, return its type
                if (symbol.IsVariable || symbol.IsProperty)
                {
                    return symbol.Type; // Return the type of the member
                }
                // If the member is a function, return its return type
                if (symbol.IsFunction)
                {
                    return symbol.Type; // Return the function's return type
                }
                // If the member is not a variable, property, or function, return an error
                errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' is not a variable, property, or function in type '{objectType}'");
                return "error"; // Member is not a variable, property, or function
            }
            private bool IsValidType(string type)
            {
                // Check if the type is a built-in type or a user-defined type
                return builtinTypes.ContainsKey(type) || currentScope.ResolveType(type) != null;
            }
            private string GetTypeName(TypeNode typeNode)
            {
                // Convert the type node to a string representation
                if (typeNode is PrimitiveTypeNode primitive)
                {
                    return primitive.Name; // Return the primitive type name
                }
                else if (typeNode is CapsuleTypeNode capsule)
                {
                    return "capsule." + capsule.Name; // Return the capsule type name
                }
                else if (typeNode is StructTypeNode struct)
                {
                    return "struct." + struct.Name; // Return the struct type name
                }
                else if (typeNode is EnumTypeNode enumType)
                {
                    return "enum." + enumType.Name; // Return the enum type name
                }
                else if (typeNode is NamespaceTypeNode ns)
                {
                    return "namespace." + ns.Name; // Return the namespace type name
                }
errors.Add($"Line {typeNode.Line}: Unknown type node '{typeNode.GetType().Name}'");
return "error"; // Unknown type node
                }
            private bool IsValidBinaryOperation(string leftType, string rightType, BinaryOperator op)
{
    // Check if the binary operation is valid for the given types
    switch (op)
    {
        case BinaryOperator.Add:
        case BinaryOperator.Subtract:
        case BinaryOperator.Multiply:
        case BinaryOperator.Divide:
            return (leftType == "int" && rightType == "int") ||
                   (leftType == "float" && rightType == "float") ||
                   (leftType == "int" && rightType == "float") ||
                   (leftType == "float" && rightType == "int");
        case BinaryOperator.Equal:
        case BinaryOperator.NotEqual:
            return leftType == rightType; // Equality checks require same type
        case BinaryOperator.LessThan:
        case BinaryOperator.GreaterThan:
        case BinaryOperator.LessThanOrEqual:
        case BinaryOperator.GreaterThanOrEqual:
            return (leftType == "int" && rightType == "int") ||
                   (leftType == "float" && rightType == "float") ||
                   (leftType == "int" && rightType == "float") ||
                   (leftType == "float" && rightType == "int");
        case BinaryOperator.And:
        case BinaryOperator.Or:
            return leftType == "bool" && rightType == "bool"; // Logical operations require boolean type
        default:
            return false; // Unknown binary operator
    }
}
private string GetResultType(string leftType, string rightType, BinaryOperator op)
{
    // Determine the result type of the binary operation
    switch (op)
    {
        case BinaryOperator.Add:
        case BinaryOperator.Subtract:
        case BinaryOperator.Multiply:
        case BinaryOperator.Divide:
            if (leftType == "int" && rightType == "int")
                return "int";
            if (leftType == "float" && rightType == "float")
                return "float";
            if ((leftType == "int" && rightType == "float") || (leftType == "float" && rightType == "int"))
                return "float";
            break;
        case BinaryOperator.Equal:
        case BinaryOperator.NotEqual:
            return "bool"; // Equality checks return boolean
        case BinaryOperator.LessThan:
        case BinaryOperator.GreaterThan:
        case BinaryOperator.LessThanOrEqual:
        case BinaryOperator.GreaterThanOrEqual:
            if (leftType == "int" && rightType == "int")
                return "bool";
            if (leftType == "float" && rightType == "float")
                return "bool";
            if ((leftType == "int" && rightType == "float") || (leftType == "float" && rightType == "int"))
                return "bool";
            break;
        case BinaryOperator.And:
        case BinaryOperator.Or:
            if (leftType == "bool" && rightType == "bool")
                return "bool"; // Logical operations return boolean
            break;
    }
    return "error"; // Invalid operation
}
private string GetUnaryResultType(string operandType, UnaryOperator op)
{
    // Determine the result type of the unary operation
    switch (op)
    {
        case UnaryOperator.Negate:
            if (operandType == "int" || operandType == "float")
                return operandType; // Negation returns the same type
            break;
        case UnaryOperator.Not:
            if (operandType == "bool")
                return "bool"; // Logical NOT returns boolean
            break;
    }
    return "error"; // Invalid unary operation
}
private bool IsAssignableType(string fromType, string toType)
{
    // Check if the fromType can be assigned to toType
    if (fromType == toType)
        return true; // Same type is always assignable
    if (toType == "int" && (fromType == "float" || fromType == "double"))
        return true; // Implicit conversion from float/double to int
    if (toType == "float" && fromType == "int")
        return true; // Implicit conversion from int to float
    if (toType.StartsWith("capsule.") && fromType.StartsWith("capsule."))
        return true; // Capsule types can be assigned to each other
    if (toType.StartsWith("struct.") && fromType.StartsWith("struct."))
        return true; // Struct types can be assigned to each other
    if (toType.StartsWith("enum.") && fromType.StartsWith("enum."))
        return true; // Enum types can be assigned to each other
    return false; // Not assignable
}
using System;
using System.Collections.Generic;
using CapsuleCompiler.AST;
using CapsuleCompiler.Symbols;
using CapsuleCompiler.Scopes;
namespace CapsuleCompiler.Semantics
{
    public class SemanticAnalyzer
    {
        private Scope currentScope;
        private readonly List<string> errors = new List<string>();
        private readonly Dictionary<string, string> builtinTypes = new Dictionary<string, string>
        {
            { "int", "int" },
            { "float", "float" },
            { "bool", "bool" },
            { "string", "string" }
        };
        public SemanticAnalyzer()
        {
            currentScope = new Scope();
        }
        public List<string> Analyze(List<DeclarationNode> declarations)
        {
            foreach (var declaration in declarations)
            {
                VisitDeclaration(declaration);
            }
            return errors;
        }
        private void VisitDeclaration(DeclarationNode declaration)
        {
            switch (declaration)
            {
                case FunctionDeclarationNode function:
                    VisitFunctionDeclaration(function);
                    break;
                case VariableDeclarationNode variable:
                    VisitVariableDeclaration(variable);
                    break;
                case NamespaceDeclarationNode ns:
                    VisitNamespaceDeclaration(ns);
                    break;
                case StructDeclarationNode structDecl:
                    VisitStructDeclaration(structDecl);
                    break;
                case EnumDeclarationNode enumDecl:
                    VisitEnumDeclaration(enumDecl);
                    break;
                default:
                    errors.Add($"Line {declaration.Line}: Unknown declaration type '{declaration.GetType().Name}'");
                    break;
            }
        }
        private void VisitFunctionDeclaration(FunctionDeclarationNode function)
        {
            // Create a new function symbol
            var symbol = new Symbol
            {
                Name = function.Name,
                Type = GetTypeName(function.ReturnType),
                Parameters = function.Parameters,
                IsVariable = false,
                IsFunction = true,
                IsType = false,
                Line = function.Line,
                Column = function.Column
            };
            // Check if the function name is valid
            if (!IsValidType(symbol.Type))
            {
                errors.Add($"Line {function.Line}: Invalid return type '{symbol.Type}' for function '{symbol.Name}'");
                return;
            }
            // Define the function in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {function.Line}: {ex.Message}");
                return;
            }
            // Enter function scope
            var functionScope = new Scope { Parent = currentScope, FunctionSymbol = symbol };
            currentScope = functionScope;
            // Visit all parameters of the function
            foreach (var param in function.Parameters)
            {
                var paramSymbol = new Symbol
                {
                    Name = param.Name,
                    Type = GetTypeName(param.Type),
                    IsVariable = true,
                    IsFunction = false,
                    IsType = false,
                    Line = param.Line,
                    Column = param.Column
                };
                // Define the parameter in the current scope
                try
                {
                    currentScope.Define(paramSymbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {param.Line}: {ex.Message}");
                }
            }
            // Visit the function body
            if (function.Body != null)
            {
                VisitBlockStatement(function.Body);
            }
            // Exit function scope
            currentScope = currentScope.Parent;
        }
        private void VisitVariableDeclaration(VariableDeclarationNode variable)
        {
            // Create a new variable symbol
            var symbol = new Symbol
            {
                Name = variable.Name,
                Type = GetTypeName(variable.Type),
                IsVariable = true,
                IsFunction = false,
                IsType = false,
                Line = variable.Line,
                Column = variable.Column
            };
            // Check if the variable type is valid
            if (!IsValidType(symbol.Type))
            {
                errors.Add($"Line {variable.Line}: Invalid type '{symbol.Type}' for variable '{symbol.Name}'");
                return;
            }
            // Define the variable in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {variable.Line}: {ex.Message}");
            }
        }
        private void VisitNamespaceDeclaration(NamespaceDeclarationNode ns)
        {
            // Create a new namespace symbol
            var symbol = new Symbol
            {
                Name = ns.Name,
                Type = "namespace",
                IsType = true,
                Line = ns.Line,
                Column = ns.Column
            };
            // Define the namespace in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {ns.Line}: {ex.Message}");
                return;
            }
            // Enter namespace scope
            var namespaceScope = new Scope { Parent = currentScope };
            currentScope = namespaceScope;
            // Visit all declarations within the namespace
            foreach (var declaration in ns.Declarations)
            {
                VisitDeclaration(declaration);
            }
            // Exit namespace scope
            currentScope = currentScope.Parent;
        }
        private void VisitStructDeclaration(StructDeclarationNode structDecl)
        {
            // Create a new struct symbol
            var symbol = new Symbol
            {
                Name = structDecl.Name,
                Type = "struct." + structDecl.Name,
                IsType = true,
                Line = structDecl.Line,
                Column = structDecl.Column
            };
            // Define the struct in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {structDecl.Line}: {ex.Message}");
                return;
            }
            // Enter struct scope
            var structScope = new Scope { Parent = currentScope };
            currentScope = structScope;
            // Visit all members of the struct
            foreach (var member in structDecl.Members)
            {
                var memberSymbol = new Symbol
                {
                    Name = member.Name,
                    Type = GetTypeName(member.Type),
                    IsVariable = true,
                    IsFunction = false,
                    IsType = false,
                    Line = member.Line,
                    Column = member.Column
                };
                // Define the member in the current scope
                try
                {
                    currentScope.Define(memberSymbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {member.Line}: {ex.Message}");
                }
            }
            // Exit struct scope
            currentScope = currentScope.Parent;
        }
        private void VisitEnumDeclaration(EnumDeclarationNode enumDecl)
        {
            // Create a new enum symbol
            var symbol = new Symbol
            {
                Name = enumDecl.Name,
                Type = "enum." + enumDecl.Name,
                IsType = true,
                Line = enumDecl.Line,
                Column = enumDecl.Column
            };
            // Define the enum in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {enumDecl.Line}: {ex.Message}");
                return;
            }
            // Enter enum scope
            var enumScope = new Scope { Parent = currentScope };
            currentScope = enumScope;
            // Visit all members of the enum
            foreach (var member in enumDecl.Members)
            {
                var memberSymbol = new Symbol
                {
                    Name = member.Name,
                    Type = "enum." + enumDecl.Name, // Enum members are of the enum type
                    IsVariable = true,
                    IsFunction = false,
                    IsType = false,
                    Line = member.Line,
                    Column = member.Column
                };
                // Define the member in the current scope
                try
                {
                    currentScope.Define(memberSymbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {member.Line}: {ex.Message}");
                }
            }
            // Exit enum scope
            currentScope = currentScope.Parent;
        }
        private void VisitBlockStatement(BlockStatementNode block)
        {
            // Enter a new scope for the block
            var blockScope = new Scope { Parent = currentScope };
            currentScope = blockScope;
            // Visit all statements in the block
            foreach (var statement in block.Statements)
            {
                VisitStatement(statement);
            }
            // Exit the block scope
            currentScope = currentScope.Parent;
        }
        private void VisitStatement(StatementNode statement)
        {
            switch (statement)
            {
                case ExpressionStatementNode exprStmt:
                    VisitExpressionStatement(exprStmt);
                    break;
                case IfStatementNode ifStmt:
                    VisitIfStatement(ifStmt);
                    break;
                case WhileStatementNode whileStmt:
                    VisitWhileStatement(whileStmt);
                    break;
                case ForStatementNode forStmt:
                    VisitForStatement(forStmt);
                    break;
                case ReturnStatementNode returnStmt:
                    VisitReturnStatement(returnStmt);
                    break;
                case ThrowStatementNode throwStmt:
                    VisitThrowStatement(throwStmt);
                    break;
                default:
                    errors.Add($"Line {statement.Line}: Unknown statement type '{statement.GetType().Name}'");
                    break;
            }
        }
        private void VisitExpressionStatement(ExpressionStatementNode exprStmt)
        {
            // Visit the expression in the statement
            string exprType = VisitExpression(exprStmt.Expression);
            // Check if the expression type is valid
            if (exprType == "error")
            {
                errors.Add($"Line {exprStmt.Line}: Invalid expression in statement");
            }
            else if (!IsValidType(exprType))
            {
                errors.Add($"Line {exprStmt.Line}: Invalid type '{exprType}' in expression statement");
            }
        }
        private void VisitIfStatement(IfStatementNode ifStmt)
        {
            // Visit the condition expression
            string conditionType = VisitExpression(ifStmt.Condition);
            // Check if the condition is a boolean expression
            if (conditionType != "bool")
            {
                errors.Add($"Line {ifStmt.Line}: If condition must be a boolean expression, but got '{conditionType}'");
            }
            // Visit the true branch
            if (ifStmt.TrueBranch != null)
            {
                VisitBlockStatement(ifStmt.TrueBranch);
            }
            // Visit the false branch if it exists
            if (ifStmt.FalseBranch != null)
            {
                VisitBlockStatement(ifStmt.FalseBranch);
            }
        }
        private void VisitWhileStatement(WhileStatementNode whileStmt)
        {
            // Visit the condition expression
            string conditionType = VisitExpression(whileStmt.Condition);
            // Check if the condition is a boolean expression
            if (conditionType != "bool")
            {
                errors.Add($"Line {whileStmt.Line}: While condition must be a boolean expression, but got '{conditionType}'");
            }
            // Visit the body of the while loop
            if (whileStmt.Body != null)
            {
                VisitBlockStatement(whileStmt.Body);
            }
        }
        private void VisitForStatement(ForStatementNode forStmt)
        {
            // Visit the initialization expression
            if (forStmt.Initialization != null)
            {
                VisitExpression(forStmt.Initialization);
            }
            // Visit the condition expression
            if (forStmt.Condition != null)
            {
                string conditionType = VisitExpression(forStmt.Condition);
                // Check if the condition is a boolean expression
                if (conditionType != "bool")
                {
                    errors.Add($"Line {forStmt.Line}: For condition must be a boolean expression, but got '{conditionType}'");
                }
            }
            // Visit the increment expression
            if (forStmt.Increment != null)
            {
                VisitExpression(forStmt.Increment);
            }
            // Visit the body of the for loop
            if (forStmt.Body != null)
            {
                VisitBlockStatement(forStmt.Body);
            }
        }
        private void VisitReturnStatement(ReturnStatementNode returnStmt)
        {
            // Check if the return statement has an expression
            if (returnStmt.Expression != null)
            {
                string returnType = VisitExpression(returnStmt.Expression);
                // Check if the return type matches the function's return type
                if (currentScope.FunctionSymbol != null && currentScope.FunctionSymbol.Type != "void"
                    && !IsAssignableType(returnType, currentScope.FunctionSymbol.Type))
                {
                    errors.Add($"Line {returnStmt.Line}: Return type '{returnType}' does not match function's return type '{currentScope.FunctionSymbol.Type}'");
                }
            }
            else
            {
                // If the function is not void, a return statement without an expression is invalid
                if (currentScope.FunctionSymbol != null && currentScope.FunctionSymbol.Type != "void")
                {
                    errors.Add($"Line {returnStmt.Line}: Return statement must have an expression in non-void function '{currentScope.FunctionSymbol.Name}'");
                }
            }
        }
        private void VisitThrowStatement(ThrowStatementNode throwStmt)
        {
            // Check if the throw statement has an expression
            if (throwStmt.Expression != null)
            {
                string throwType = VisitExpression(throwStmt.Expression);
                // Check if the throw type is valid (e.g., an exception type)
                if (!IsValidType(throwType) || !throwType.StartsWith("exception."))
                {
                    errors.Add($"Line {throwStmt.Line}: Invalid throw type '{throwType}'");
                }
            }
            else
            {
                errors.Add($"Line {throwStmt.Line}: Throw statement must have an expression");
            }
        }
        private string VisitExpression(ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralNode literal:
                    return VisitLiteral(literal);
                case BinaryExpressionNode binary:
                    return VisitBinaryExpression(binary);
                case UnaryExpressionNode unary:
                    return VisitUnaryExpression(unary);
                case FunctionCallNode functionCall:
                    return VisitFunctionCall(functionCall);
                case MemberAccessNode memberAccess:
                    return VisitMemberAccess(memberAccess);
                default:
                    errors.Add($"Line {expression.Line}: Unknown expression type '{expression.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitLiteral(LiteralNode literal)
        {
            // Check the type of the literal and return its type
            switch (literal.Value)
            {
                case int _:
                    return "int"; // Integer literal
                case float _:
                    return "float"; // Float literal
                case bool _:
                    return "bool"; // Boolean literal
                case string _:
                    return "string"; // String literal
                default:
                    errors.Add($"Line {literal.Line}: Unknown literal type '{literal.Value.GetType().Name}'");
                    return "error"; // Unknown literal type
            }
        }
        private string VisitBinaryExpression(BinaryExpressionNode binary)
        {
            // Visit the left and right expressions
            string leftType = VisitExpression(binary.Left);
            string rightType = VisitExpression(binary.Right);
            // Check if the left and right types are valid
            if (leftType == "error" || rightType == "error")
            {
                errors.Add($"Line {binary.Line}: Invalid types for binary operation '{binary.Operator}'");
                return "error";
            }
            // Check if the binary operation is valid for the given types
            if (!IsValidBinaryOperation(leftType, rightType, binary.Operator))
            {
                errors.Add($"Line {binary.Line}: Invalid binary operation '{binary.Operator}' for types '{leftType}' and '{rightType}'");
                return "error";
            }
            // If the operation is valid, return the result type
            return GetResultType(leftType, rightType, binary.Operator);
        }
        private string VisitUnaryExpression(UnaryExpressionNode unary)
        {
            // Visit the operand expression
            string operandType = VisitExpression(unary.Operand);
            // Check if the operand type is valid
            if (operandType == "error")
            {
                errors.Add($"Line {unary.Line}: Invalid operand type for unary operation '{unary.Operator}'");
                return "error";
            }
            // Check if the unary operation is valid for the operand type
            string resultType = GetUnaryResultType(operandType, unary.Operator);
            if (resultType == "error")
            {
                errors.Add($"Line {unary.Line}: Invalid unary operation '{unary.Operator}' for type '{operandType}'");
                return "error";
            }
            // If the operation is valid, return the result type
            return resultType;
        }
        private string VisitFunctionCall(FunctionCallNode functionCall)
        {
            // Resolve the function symbol in the current scope
            var symbol = currentScope.ResolveFunction(functionCall.FunctionName);
            if (symbol == null)
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.FunctionName}' not found");
                return "error";
            }
            // Check if the function call has the correct number of arguments
            if (symbol.Parameters.Count != functionCall.Arguments.Count)
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.FunctionName}' expects {symbol.Parameters.Count} arguments, but got {functionCall.Arguments.Count}");
                return "error";
            }
            // Check the types of the arguments
            for (int i = 0; i < symbol.Parameters.Count; i++)
            {
                string argType = VisitExpression(functionCall.Arguments[i]);
                if (argType == "error")
                {
                    errors.Add($"Line {functionCall.Line}: Invalid argument type for parameter '{symbol.Parameters[i].Name}'");
                    return "error";
                }
                // Check if the argument type is assignable to the parameter type
                if (!IsAssignableType(argType, symbol.Parameters[i].Type))
                {
                    errors.Add($"Line {functionCall.Line}: Argument type '{argType}' is not assignable to parameter type '{symbol.Parameters[i].Type}'");
                    return "error";
                }
            }
            // If the function call is valid, return the function's return type
            return symbol.Type;
        }
        private string VisitMemberAccess(MemberAccessNode memberAccess)
        {
            // Resolve the type of the object being accessed
            string objectType = VisitExpression(memberAccess.Object);
            if (objectType == "error")
            {
                errors.Add($"Line {memberAccess.Line}: Invalid object type for member access");
                return "error";
            }
            // Check if the member exists in the object's type
            var memberSymbol = currentScope.ResolveMember(objectType, memberAccess.MemberName);
            if (memberSymbol == null)
            {
                errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in type '{objectType}'");
                return "error";
            }
            // Return the type of the member
            return memberSymbol.Type;
        }
        private bool IsValidType(string type)
        {
            // Check if the type is a valid built-in type or a user-defined type
            return builtinTypes.ContainsKey(type) || type.StartsWith("capsule.") || type.StartsWith("struct.") || type.StartsWith("enum.");
        }
        private bool IsValidBinaryOperation(string leftType, string rightType, BinaryOperator op)
        {
            // Check if the binary operation is valid for the given types
            switch (op)
            {
                case BinaryOperator.Add:
                case BinaryOperator.Subtract:
                case BinaryOperator.Multiply:
                case BinaryOperator.Divide:
                    return (leftType == "int" && rightType == "int") ||
                           (leftType == "float" && rightType == "float") ||
                           (leftType == "int" && rightType == "float") ||
                           (leftType == "float" && rightType == "int");
                case BinaryOperator.Equal:
                case BinaryOperator.NotEqual:
                    return (leftType == "int" && rightType == "int") ||
                           (leftType == "float" && rightType == "float") ||
                           (leftType == "bool" && rightType == "bool") ||
                           (leftType == "string" && rightType == "string") ||
                           (leftType.StartsWith("capsule.") && rightType.StartsWith("capsule.")) ||
                           (leftType.StartsWith("struct.") && rightType.StartsWith("struct.")) ||
                           (leftType.StartsWith("enum.") && rightType.StartsWith("enum."));
                case BinaryOperator.LessThan:
                case BinaryOperator.GreaterThan:
                case BinaryOperator.LessThanOrEqual:
                case BinaryOperator.GreaterThanOrEqual:
                    return (leftType == "int" && rightType == "int") ||
                           (leftType == "float" && rightType == "float") ||
                           (leftType == "int" && rightType == "float") ||
                           (leftType == "float" && rightType == "int");
                case BinaryOperator.And:
                case BinaryOperator.Or:
                    return leftType == "bool" && rightType == "bool"; // Logical operations only valid for boolean types
                default:
                    return false; // Invalid operator
            }
            private string GetTypeName(TypeNode typeNode)
        {
            // Get the type name from the type node
            if (typeNode is PrimitiveTypeNode primitive)
            {
                return primitive.Name; // Return primitive type name
            }
            else if (typeNode is UserDefinedTypeNode userDefined)
            {
                return userDefined.Name; // Return user-defined type name
            }
            else if (typeNode is ArrayTypeNode array)
            {
                return $"{GetTypeName(array.ElementType)}[]"; // Return array type
            }
            else if (typeNode is CapsuleTypeNode capsule)
            {
                return $"capsule.{capsule.Name}"; // Return capsule type
            }
            else if (typeNode is StructTypeNode struct)
                {
                    return $"struct.{struct.Name}"; // Return struct type
                }
                else if (typeNode is EnumTypeNode enumType)
                {
                    return $"enum.{enumType.Name}"; // Return enum type
    }
                return "error"; // Unknown type node
            }
private string GetResultType(string leftType, string rightType, BinaryOperator op)
{
    // Determine the result type based on the operator and operand types
    switch (op)
    {
        case BinaryOperator.Add:
        case BinaryOperator.Subtract:
        case BinaryOperator.Multiply:
        case BinaryOperator.Divide:
            if (leftType == "int" && rightType == "int")
                return "int";
            if (leftType == "float" && rightType == "float")
                return "float";
            if ((leftType == "int" && rightType == "float") || (leftType == "float" && rightType == "int"))
                return "float"; // Mixed types result in float
            break;
        case BinaryOperator.Equal:
        case BinaryOperator.NotEqual:
            return "bool"; // Comparison operations return boolean
        case BinaryOperator.LessThan:
        case BinaryOperator.GreaterThan:
        case BinaryOperator.LessThanOrEqual:
        case BinaryOperator.GreaterThanOrEqual:
            if (leftType == "int" && rightType == "int")
                return "bool";
            if (leftType == "float" && rightType == "float")
                return "bool";
            if ((leftType == "int" && rightType == "float") || (leftType == "float" && rightType == "int"))
                return "bool"; // Mixed types result in boolean
            break;
        case BinaryOperator.And:
        case BinaryOperator.Or:
            if (leftType == "bool" && rightType == "bool")
                return "bool"; // Logical operations return boolean
            break;
    }
    return "error"; // Invalid binary operation
}
private string GetUnaryResultType(string operandType, UnaryOperator op)
{
    // Determine the result type based on the unary operator and operand type
    switch (op)
    {
        case UnaryOperator.Negate:
            if (operandType == "int" || operandType == "float")
                return operandType; // Negation returns the same type
            break;
        case UnaryOperator.Not:
            if (operandType == "bool")
                return "bool"; // Logical NOT returns boolean
            break;
    }
    return "error"; // Invalid unary operation
}
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Collections.Generic;
using MyLanguage.AST;
using MyLanguage.Symbols;
using MyLanguage.Scopes;
using MyLanguage.Errors;
namespace MyLanguage.SemanticAnalysis
{
    public class SemanticAnalyzer
    {
        private Scope currentScope;
        private List<string> errors;
        private readonly Dictionary<string, string> builtinTypes = new Dictionary<string, string>
        {
            { "int", "int" },
            { "float", "float" },
            { "bool", "bool" },
            { "string", "string" },
            { "void", "void" }
        };
        public SemanticAnalyzer()
        {
            currentScope = new Scope();
            errors = new List<string>();
        }
        public List<string> Analyze(List<DeclarationNode> declarations)
        {
            foreach (var declaration in declarations)
            {
                VisitDeclaration(declaration);
            }
            return errors;
        }
        private void VisitDeclaration(DeclarationNode declaration)
        {
            switch (declaration)
            {
                case FunctionDeclarationNode functionDecl:
                    VisitFunctionDeclaration(functionDecl);
                    break;
                case VariableDeclarationNode variableDecl:
                    VisitVariableDeclaration(variableDecl);
                    break;
                case NamespaceDeclarationNode namespaceDecl:
                    VisitNamespaceDeclaration(namespaceDecl
                        break;
                case StructDeclarationNode structDecl:
                    VisitStructDeclaration(structDecl);
                    break;
                case EnumDeclarationNode enumDecl:
                    VisitEnumDeclaration(enumDecl);
                    break;
                default:
                    errors.Add($"Line {declaration.Line}: Unknown declaration type '{declaration.GetType().Name}'");
                    break;
            }
        }
        private void VisitFunctionDeclaration(FunctionDeclarationNode function)
        {
            // Create a new function symbol
            var symbol = new Symbol
            {
                Name = function.Name,
                Type = GetTypeName(function.ReturnType),
                IsFunction = true,
                IsVariable = false,
                IsType = false,
                Line = function.Line,
                Column = function.Column
            };
            // Check if the return type is valid
            if (!IsValidType(symbol.Type))
            {
                errors.Add($"Line {function.Line}: Invalid return type '{symbol.Type}' for function '{symbol.Name}'");
                return;
            }
            // Define the function in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {function.Line}: {ex.Message}");
                return;
            }
            // Enter function scope
            var functionScope = new Scope { Parent = currentScope, FunctionSymbol = symbol };
            currentScope = functionSco
                pe;
            // Visit all parameters of the function
            foreach (var parameter in function.Parameters)
            {
                var paramSymbol = new Symbol
                {
                    Name = parameter.Name,
                    Type = GetTypeName(parameter.Type),
                    IsVariable = true,
                    IsFunction = false,
                    IsType = false,
                    Line = parameter.Line,
                    Column = parameter.Column
                };
                // Check if the parameter type is valid
                if (!IsValidType(paramSymbol.Type))
                {
                    errors.Add($"Line {parameter.Line}: Invalid type '{paramSymbol.Type}' for parameter '{paramSymbol.Name}'");
                    continue;
                }
                // Define the parameter in the current scope
                try
                {
                    currentScope.Define(paramSymbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {parameter.Line}: {ex.Message}");
                }
            }
            // Visit the function body
            if (function.Body != null)
            {
                VisitBlockStatement(function.Body);
            }
            // Exit function scope
            currentScope = currentScope.Parent;
        }
        private void VisitVariableDeclaration(VariableDeclarationNode variable)
        {
            // Create a new variable symbol
            var symbol = new Symbol
            {
                Name = variable.Name,
                Type = GetTypeName(variable.Type),
                IsVariable = true,
                IsFunction = false,
                IsType = false,
                Line = variable.Line,
                Column = variable.Column
            };
            // Check if the variable type is valid
            if (!IsValidType(symbol.Type))
            {
                errors.Add($"Line {variable.Line}: Invalid type '{symbol.Type}' for variable '{symbol.Name}'");
                return;
            }
            // Define the variable in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {variable.Line}: {ex.Message}");
            }
        }
        private void VisitNamespaceDeclaration(NamespaceDeclarationNode namespaceDecl)
        {
            // Create a new namespace symbol
            var symbol = new Symbol
            {
                Name = namespaceDecl.Name,
                Type = "namespace." + namespaceDecl.Name,
                IsType = true,
                Line = namespaceDecl.Line,
                Column = namespaceDecl.Column
            };
            // Define the namespace in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {namespaceDecl.Line}: {ex.Message}");
                return;
            }
            // Enter namespace scope
            var namespaceScope = new Scope { Parent = currentScope };
            currentScope = namespaceScope;
            // Visit all declarations within the namespace
            foreach (var decl in namespaceDecl.Declarations)
            {
                VisitDeclaration(decl);
            }
            // Exit namespace scope
            currentScope = currentScope.Parent;
        }
        private void VisitStructDeclaration(StructDeclarationNode structDecl)
        {
            // Create a new struct symbol
            var symbol = new Symbol
            {
                Name = structDecl.Name,
                Type = "struct." + structDecl.Name,
                IsType = true,
                Line = structDecl.Line,
                Column = structDecl.Column
            };
            // Define the struct in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {structDecl.Line}: {ex.Message}");
                return;
            }
            // Enter struct scope
            var structScope = new Scope { Parent = currentScope };
            currentScope = structScope;
            // Visit all members of the struct
            foreach (var member in structDecl.Members)
            {
                var memberSymbol = new Symbol
                {
                    Name = member.Name,
                    Type = GetTypeName(member.Type),
                    IsVariable = true,
                    IsFunction = false,
                    IsType = false,
                    Line = member.Line,
                    Column = member.Column
                };
                // Check if the member type is valid
                if (!IsValidType(memberSymbol.Type))
                {
                    errors.Add($"Line {member.Line}: Invalid type '{memberSymbol.Type}' for member '{memberSymbol.Name}'");
                    continue;
                }
                // Define the member in the current scope
                try
                {
                    currentScope.Define(memberSymbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"

                        Line { member.Line}: { ex.Message}
                    ");
                        }
            }
            // Exit struct scope
            currentScope = currentScope.Parent;
        }

        private void VisitEnumDeclaration(EnumDeclarationNode enumDecl)
        {
            // Create a new enum symbol
            var symbol = new Symbol
            {
                Name = enumDecl.Name,
                Type = "enum." + enumDecl.Name,
                IsType = true,
                Line = enumDecl.Line,
                Column = enumDecl.Column
            };
            // Define the enum in the current scope
            try
            {
                currentScope.Define(symbol);
            }
            catch (Exception ex)
            {
                errors.Add($"Line {enumDecl.Line}: {ex.Message}");
                return;
            }
            // Enter enum scope
            var enumScope = new Scope { Parent = currentScope };
            currentScope = enumScope;
            // Visit all members of the enum
            foreach (var member in enumDecl.Members)
            {
                var memberSymbol = new Symbol
                {
                    Name = member.Name,
                    Type = "int", // Enum members are typically integers
                    IsVariable = true,
                    IsFunction = false,
                    IsType = false,
                    Line = member.Line,
                    Column = member.Column
                };
                // Define the member in the current scope
                try
                {
                    currentScope.Define(memberSymbol);
                }
                catch (Exception ex)
                {
                    errors.Add($"Line {member.Line}: {ex.Message}");
                }
            }
            // Exit enum scope
            currentScope = currentScope.Parent;
        }
        private void VisitBlockStatement(BlockStatementNode block)
        {
            // Enter a new scope for the block
            var blockScope = new Scope { Parent = currentScope };
            currentScope = blockScope;
            // Visit all statements in the block
            foreach (var statement in block.Statements)
            {
                VisitStatement(statement);
            }
            // Exit the block scope
            currentScope = currentScope.Parent;
        }
        private void VisitStatement(StatementNode statement)
        {
            switch (statement)
            {
                case IfStatementNode ifStmt:
                    VisitIfStatement(ifStmt);
                    break;
                case WhileStatementNode whileStmt:
                    VisitWhileStatement(whileStmt);
                    break;
                case ForStatementNode forStmt:
                    VisitForStatement(forStmt);
                    break;
                case ReturnStatementNode returnStmt:
                    VisitReturnStatement(returnStmt);
                    break;
                case ThrowStatementNode throwStmt:
                    VisitThrowStatement(throwStmt);
                    break;
                case ExpressionStatementNode exprStmt:
                    VisitExpression(exprStmt.Expression);
                    break;
                default:
                    errors.Add($"Line {statement.Line}: Unknown statement type '{statement.GetType().Name}'");
                    break;
            }
        }
        private void VisitIfStatement(IfStatementNode ifStmt)
        {
            // Visit the condition expression
            string conditionType = VisitExpression(ifStmt.Condition);
            // Check if the condition is a boolean expression
            if (conditionType != "bool")
            {
                errors.Add($"Line {ifStmt.Line}: If condition must be a boolean expression, but got '{conditionType}'");
            }
            // Visit the true branch
            if (ifStmt.TrueBranch != null)
            {
                VisitBlockStatement(ifStmt.TrueBranch);
            }
            // Visit the false branch if it exists
            if (ifStmt.FalseBranch != null)
            {
                VisitBlockStatement(ifStmt.FalseBranch);
            }
        }
        private void VisitWhileStatement(WhileStatementNode whileStmt)
        {
            // Visit the condition expression
            string conditionType = VisitExpression(whileStmt.Condition);
            // Check if the condition is a boolean expression
            if (conditionType != "bool")
            {
                errors.Add($"Line {whileStmt.Line}: While condition must be a boolean expression, but got '{conditionType}'");
            }
            // Visit the body of the while loop
            if (whileStmt.Body != null)
            {
                VisitBlockStatement(whileStmt.Body);
            }
        }
        private void VisitForStatement(ForStatementNode forStmt)
        {
            // Visit the initialization expression
            if (forStmt.Initialization != null)
            {
                VisitExpression(forStmt.Initialization);
            }
            // Visit the condition expression
            string conditionType = VisitExpression(forStmt.Condition);
            // Check if the condition is a boolean expression
            if (conditionType != "bool")
            {
                errors.Add($"Line {forStmt.Line}: For loop condition must be a boolean expression, but got '{conditionType}'");
            }
            // Visit the increment expression
            if (forStmt.Increment != null)
            {
                VisitExpression(forStmt.Increment);
            }
            // Visit the body of the for loop
            if (forStmt.Body != null)
            {
                VisitBlockStatement(forStmt.Body);
            }
        }
        private void VisitReturnStatement(ReturnStatementNode returnStmt)
        {
            // Check if the return statement has an expression
            if (returnStmt.Expression != null)
            {
                string returnType = VisitExpression(returnStmt.Expression);
                // Check if the return type matches the function's return type
                if (currentScope.FunctionSymbol != null && returnType != currentScope.FunctionSymbol.Type)
                {
                    errors.Add($"Line {returnStmt.Line}: Return type '{returnType}' does not match function's return type '{currentScope.FunctionSymbol.Type}'");
                }
            }
            else
            {
                // If the function expects a return type, but no expression is provided
                if (currentScope.FunctionSymbol != null && currentScope.FunctionSymbol.Type != "void")
                {
                    errors.Add($"Line {returnStmt.Line}: Function '{currentScope.FunctionSymbol.Name}' expects a return value of type '{currentScope.FunctionSymbol.Type}'");
                }
            }
        }
        private void VisitThrowStatement(ThrowStatementNode throwStmt)
        {
            // Visit the expression being thrown
            string throwType = VisitExpression(throwStmt.Expression);
            // Check if the thrown type is valid
            if (!IsValidType(throwType))
            {
                errors.Add($"Line {throwStmt.Line}: Cannot throw type '{throwType}'");
            }
        }
        private string VisitExpression(ExpressionNode expression)
        {
            // Check the type of the expression and visit accordingly
            switch (expression)
            {
                case LiteralNode literal:
                    return VisitLiteral(literal);
                case BinaryExpressionNode binary:
                    return VisitBinaryExpression(binary);
                case UnaryExpressionNode unary:
                    return VisitUnaryExpression(unary);
                case FunctionCallNode functionCall:
                    return VisitFunctionCall(functionCall);
                case MemberAccessNode memberAccess:
                    return VisitMemberAccess(memberAccess);
                default:
                    errors.Add($"Line {expression.Line}: Unknown expression type '{expression.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitLiteral(LiteralNode literal)
        {
            // Return the type of the literal based on its value
            switch (literal.Value)
            {
                case int _:
                    return "int";
                case float _:
                    return "float";
                case bool _:
                    return "bool";
                case string _:
                    return "string";
                default:
                    errors.Add($"Line {literal.Line}: Unknown literal type '{literal.Value.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitBinaryExpression(BinaryExpressionNode binary)
        {
            // Visit the left and right expressions
            string leftType = VisitExpression(binary.Left);
            string rightType = VisitExpression(binary.Right);
            // Check if either left or right type is invalid
            if (leftType == "error" || rightType == "error")
            {
                errors.Add($"Line {binary.Line}: Invalid operand types for binary operation '{binary.Operator}'");
                return "error";
            }
            // Check if the binary operation is valid for the operand types
            if (!IsValidBinaryOperation(leftType, rightType, binary.Operator))
            {
                errors.Add($"Line {binary.Line}: Invalid binary operation '{binary.Operator}' for types '{leftType}' and '{rightType}'");
                return "error";
            }
            // Get the result type of the binary operation
            string resultType = GetResultType(leftType, rightType, binary.Operator);
            if (resultType == "error")
            {
                errors.Add($"Line {binary.Line}: Invalid result type for binary operation '{binary.Operator}' with types '{leftType}' and '{rightType}'");
                return "error";
            }
            return resultType;
        }
        private string VisitUnaryExpression(UnaryExpressionNode unary)
        {
            // Visit the operand expression
            string operandType = VisitExpression(unary.Operand);
            // Check if the operand type is invalid
            if (operandType == "error")
            {
                errors.Add($"Line {unary.Line}: Invalid operand type for unary operation '{unary.Operator}'");
                return "error";
            }
            // Check if the unary operation is valid for the operand type
            string resultType = GetUnaryResultType(operandType, unary.Operator);
            if (resultType == "error")
            {
                errors.Add($"Line {unary.Line}: Invalid unary operation '{unary.Operator}' for type '{operandType}'");
                return "error";
            }
            return resultType;
        }
        private string VisitFunctionCall(FunctionCallNode functionCall)
        {
            // Resolve the function symbol in the current scope
            var symbol = currentScope.ResolveFunction(functionCall.Name);
            if (symbol == null)
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' not found");
                return "error";
            }
            // Check if the number of arguments matches the function's parameters
            if (functionCall.Arguments.Count != symbol.Parameters.Count)
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' expects {symbol.Parameters.Count} arguments, but got {functionCall.Arguments.Count}");
                return "error";
            }
            // Check each argument type against the corresponding parameter type
            for (int i = 0; i < functionCall.Arguments.Count; i++)
            {
                string argType = VisitExpression(functionCall.Argum

                    ents[i]);
                string paramType = symbol.Parameters[i].Type;
                if (argType == "error" || paramType == "error")
                {
                    errors.Add($"Line {functionCall.Line}: Invalid argument type for parameter '{symbol.Parameters[i].Name}' in function '{functionCall.Name}'");
                    return "error";
                }
                if (!IsValidType(argType) || !IsValidType(paramType))
                {
                    errors.Add($"Line {functionCall.Line}: Invalid type '{argType}' for argument '{i + 1}' in function '{functionCall.Name}'");
                    return "error";
                }
                if (argType != paramType)
                {
                    errors.Add($"Line {functionCall.Line}: Argument '{i + 1}' type '{argType}' does not match parameter type '{paramType}' in function '{functionCall.Name}'");
                    return "error";
                }
            }
            // Return the function's return type
            return symbol.Type;
        }
        private string VisitMemberAccess(MemberAccessNode memberAccess)
        {
            // Resolve the type of the object being accessed
            string objectType = VisitExpression(memberAccess.Object);
            // Check if the object type is valid
            if (objectType == "error")
            {
                errors.Add($"Line {memberAccess.Line}: Invalid object type for member access '{memberAccess.MemberName}'");
                return "error";
            }
            // Check if the member exists in the object's type
            var memberSymbol = currentScope.ResolveMember(objectType, memberAccess.MemberName);
            if (memberSymbol == null)
            {
                errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in type '{objectType}'");
                return "error";
            }
            // Return the member's type
            return memberSymbol.Type;
        }
        private bool IsValidType(string type)
        {
            // Check if the type is a built-in type or a user-defined type
            return builtinTypes.ContainsKey(type) || currentScope.ResolveType(type) != null;
        }
        private bool IsValidBinaryOperation(string leftType, string rightType, BinaryOperator op)
        {
            // Check if the binary operation is valid for the operand types
            switch (op)
            {
                case BinaryOperator.Add:
                case BinaryOperator.Subtract:
                case BinaryOperator.Multiply:
                case BinaryOperator.Divide:
                    return (leftType == "int" && rightType == "int") ||
                           (leftType == "float" && rightType == "float") ||
                           (leftType == "int" && rightType == "float") ||
                           (leftType == "float" && rightType == "int");
                case BinaryOperator.Equal:
                case BinaryOperator.NotEqual:
                    return (leftType == "int" && rightType == "int") ||
                           (leftType == "float" && rightType == "float") ||
                           (leftType == "bool" && rightType == "bool") ||
                           (leftType == "string" && rightType == "string");
                case BinaryOperator.LessThan:
                case BinaryOperator.GreaterThan:
                case BinaryOperator.LessThanOrEqual:
                case BinaryOperator.GreaterThanOrEqual:
                    return (leftType == "int" && rightType == "int") ||
                           (leftType == "float" && rightType == "float") ||
                           (leftType == "int" && rightType == "float") ||
                           (leftType == "float" && rightType == "int");
                case BinaryOperator.And:
                case BinaryOperator.Or:
                    return leftType == "bool" && rightType == "bool"; // Logical operations require boolean types
                default:
                    return false; // Invalid operator
                    }
            }
        private string GetTypeName(TypeNode typeNode)
            {
            // Get the type name from the type node
            if (typeNode is PrimitiveTypeNode primitive)
            {
                return primitive.Name; // Return primitive type name
            }
            else if (typeNode is UserDefinedTypeNode userDefined)
            {
                return userDefined.Name; // Return user-defined type name
            }
            else if (typeNode is ArrayTypeNode array)
            {
                return $"{GetTypeName(array.ElementType)}[]"; // Return array type
            }
            else if (typeNode is CapsuleTypeNode capsule)
            {
                return $"capsule.{capsule.Name}"; // Return capsule type
            }
            else if (typeNode is StructTypeNode struct)
            {
                return $"struct.{struct.Name}"; // Return struct type
            }
            else if (typeNode is EnumTypeNode enumType)
            {
                return $"enum.{enumType.Name}"; // Return enum type
            }
            return "error"; // Unknown type node
        }
    }
        private bool IsValidType(string type)
        {
            // Check if the type is a built-in type or a user-defined type
            return builtinTypes.ContainsKey(type) || currentScope.ResolveType(type) != null;
        }
    }
                return "error";
            }
        }
    }
// End of the code snippet
using System;
using System.Collections.Generic;
using MyLanguage.AST;
using MyLanguage.Symbols;
using MyLanguage.Scopes;
using MyLanguage.Errors;
using MyLanguage.Types;
using MyLanguage.Expressions;
using MyLanguage.Statements;
using MyLanguage.Declarations;
using MyLanguage.Operators;
public class CodeGenerator
{
    private Scope currentScope;
    private List<string> errors;
    public CodeGenerator()
    {
        currentScope = new Scope();
        errors = new List<string>();
    }
    public List<string> Generate(List<DeclarationNode> declarations)
    {
        foreach (var declaration in declarations)
        {
            VisitDeclaration(declaration);
        }
        return errors;
    }
    private void VisitDeclaration(DeclarationNode declaration)
    {
        switch (declaration)
        {
            case FunctionDeclarationNode functionDecl:
                VisitFunctionDeclaration(functionDecl);
                break;
            case VariableDeclarationNode variableDecl:
                VisitVariableDeclaration(variableDecl);
                break;
            case NamespaceDeclarationNode namespaceDecl:
                VisitNamespaceDeclaration(namespaceDecl);
                break;
            case StructDeclarationNode structDecl:
                VisitStructDeclaration(structDecl);
                break;
            case EnumDeclarationNode enumDecl:
                VisitEnumDeclaration(enumDecl);
                break;
            default:
                errors.Add($"Line {declaration.Line}: Unknown declaration type '{declaration.GetType().Name}'");
                break;
        }
    }

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace PhoenixCompiler
{
    public class CodeGenerator
    {
        private StringBuilder code;
        private Dictionary<string, string> constants;
        private List<OptimizationPass> optimizationPasses;
        private ProfileData profileData;
        private int labelCounter;
        private Dictionary<string, int> variableOffsets;
        private int stackOffset;

        public CodeGenerator()
        {
            code = new StringBuilder();
            constants = new Dictionary<string, string>();
            optimizationPasses = new List<OptimizationPass>();
            profileData = new ProfileData();
            labelCounter = 0;
            variableOffsets = new Dictionary<string, int>();
            stackOffset = 0;

            InitializeOptimizationPasses();
        }

        private void InitializeOptimizationPasses()
        {
            optimizationPasses.Add(new ConstantFoldingPass());
            optimizationPasses.Add(new PeepholeOptimizationPass());
            optimizationPasses.Add(new LoopUnrollingPass());
            optimizationPasses.Add(new TailCallOptimizationPass());
            optimizationPasses.Add(new ProfileGuidedOptimizationPass(profileData));
        }

        public string Generate(ProgramNode program, string outputPath)
        {
            // Generate initial x64 assembly
            GenerateProgram(program);

            // Apply optimization passes
            var initialCode = code.ToString();
            var optimizedCode = ApplyOptimizations(initialCode, program);

            // Write to file
            File.WriteAllText(outputPath.Replace(".exe", ".asm"), optimizedCode);

            // Compile assembly to executable (simplified)
            CompileAssembly(outputPath.Replace(".exe", ".asm"), outputPath);

            return optimizedCode;
        }

        private string ApplyOptimizations(string initialCode, ProgramNode program)
        {
            var currentCode = initialCode;
            var instructions = ParseInstructions(currentCode);

            foreach (var pass in optimizationPasses)
            {
                instructions = pass.Optimize(instructions, program);
            }

            return GenerateCodeFromInstructions(instructions);
        }

        private void GenerateProgram(ProgramNode program)
        {
            EmitHeader();

            foreach (var declaration in program.Declarations)
            {
                GenerateDeclaration(declaration);
            }

            EmitFooter();
        }

        private void EmitHeader()
        {
            code.AppendLine("section .text");
            code.AppendLine("global _start");
            code.AppendLine();
            code.AppendLine("_start:");
            code.AppendLine("    ; Phoenix ProLang Generated Code");
        }

        private void EmitFooter()
        {
            code.AppendLine("    ; Exit program");
            code.AppendLine("    mov rax, 60    ; sys_exit");
            code.AppendLine("    mov rdi, 0     ; exit status");
            code.AppendLine("    syscall");
        }

        private void GenerateDeclaration(DeclarationNode declaration)
        {
            switch (declaration)
            {
                case FunctionDeclarationNode function:
                    GenerateFunction(function);
                    break;
                case VariableDeclarationNode variable:
                    GenerateVariable(variable);
                    break;
                case CapsuleDeclarationNode capsule:
                    GenerateCapsule(capsule);
                    break;
                default:
                    // Handle other declarations
                    break;
            }
        }

        private void GenerateFunction(FunctionDeclarationNode function)
        {
            code.AppendLine($"{function.Name}:");
            code.AppendLine("    push rbp");
            code.AppendLine("    mov rbp, rsp");

            // Allocate space for local variables
            var localSize = CalculateLocalVariableSpace(function.Body);
            if (localSize > 0)
            {
                code.AppendLine($"    sub rsp, {localSize}");
            }

            // Generate function body
            if (function.Body != null)
            {
                GenerateStatement(function.Body);
            }

            // Function epilogue
            code.AppendLine($"{function.Name}_end:");
            code.AppendLine("    mov rsp, rbp");
            code.AppendLine("    pop rbp");
            code.AppendLine("    ret");
            code.AppendLine();
        }

        private void GenerateVariable(VariableDeclarationNode variable)
        {
            // Allocate space on stack for variable
            stackOffset += 8; // 64-bit variables
            variableOffsets[variable.Name] = stackOffset;

            if (variable.Initializer != null)
            {
                GenerateExpression(variable.Initializer);
                code.AppendLine($"    mov [rbp-{stackOffset}], rax");
            }
        }

        private void GenerateCapsule(CapsuleDeclarationNode capsule)
        {
            code.AppendLine($"; Capsule: {capsule.Name}");
            foreach (var member in capsule.Members)
            {
                GenerateDeclaration(member);
            }
        }

        private void GenerateStatement(StatementNode statement)
        {
            switch (statement)
            {
                case BlockStatementNode block:
                    GenerateBlockStatement(block);
                    break;
                case ExpressionStatementNode expr:
                    GenerateExpression(expr.Expression);
                    break;
                case IfStatementNode ifStmt:
                    GenerateIfStatement(ifStmt);
                    break;
                case LoopStatementNode loop:
                    GenerateLoopStatement(loop);
                    break;
                case ReturnStatementNode ret:
                    GenerateReturnStatement(ret);
                    break;
                default:
                    // Handle other statements
                    break;
            }
        }

        private void GenerateBlockStatement(BlockStatementNode block)
        {
            foreach (var statement in block.Statements)
            {
                GenerateStatement(statement);
            }
        }

        private void GenerateIfStatement(IfStatementNode ifStmt)
        {
            var elseLabel = GetNextLabel("else");
            var endLabel = GetNextLabel("endif");

            GenerateExpression(ifStmt.Condition);
            code.AppendLine("    cmp rax, 0");
            code.AppendLine($"    je {elseLabel}");

            GenerateStatement(ifStmt.ThenStatement);
            code.AppendLine($"    jmp {endLabel}");

            code.AppendLine($"{elseLabel}:");
            if (ifStmt.ElseStatement != null)
            {
                GenerateStatement(ifStmt.ElseStatement);
            }

            code.AppendLine($"{endLabel}:");
        }

        private void GenerateLoopStatement(LoopStatementNode loop)
        {
            var loopLabel = GetNextLabel("loop");
            var endLabel = GetNextLabel("endloop");

            code.AppendLine($"{loopLabel}:");
            GenerateExpression(loop.Condition);
            code.AppendLine("    cmp rax, 0");
            code.AppendLine($"    je {endLabel}");

            GenerateStatement(loop.Body);
            code.AppendLine($"    jmp {loopLabel}");
            code.AppendLine($"{endLabel}:");
        }

        private void GenerateReturnStatement(ReturnStatementNode ret)
        {
            if (ret.Expression != null)
            {
                GenerateExpression(ret.Expression);
            }
            code.AppendLine("    mov rsp, rbp");
            code.AppendLine("    pop rbp");
            code.AppendLine("    ret");
        }

        private void GenerateExpression(ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralExpressionNode literal:
                    GenerateLiteral(literal);
                    break;
                case IdentifierExpressionNode identifier:
                    GenerateIdentifier(identifier);
                    break;
                case BinaryExpressionNode binary:
                    GenerateBinaryExpression(binary);
                    break;
                case UnaryExpressionNode unary:
                    GenerateUnaryExpression(unary);
                    break;
                case FunctionCallExpressionNode call:
                    GenerateFunctionCall(call);
                    break;
                default:
                    // Handle other expressions
                    break;
            }
        }

        private void GenerateLiteral(LiteralExpressionNode literal)
        {
            switch (literal.Type)
            {
                case TokenType.Integer:
                    code.AppendLine($"    mov rax, {literal.Value}");
                    break;
                case TokenType.String:
                    var label = GetNextLabel("str");
                    code.AppendLine($"    mov rax, {label}");
                    // Store string in data section (simplified)
                    break;
                default:
                    code.AppendLine($"    mov rax, {literal.Value}");
                    break;
            }
        }

        private void GenerateIdentifier(IdentifierExpressionNode identifier)
        {
            if (variableOffsets.ContainsKey(identifier.Name))
            {
                var offset = variableOffsets[identifier.Name];
                code.AppendLine($"    mov rax, [rbp-{offset}]");
            }
            else
            {
                code.AppendLine($"    ; Unknown identifier: {identifier.Name}");
            }
        }

        private void GenerateBinaryExpression(BinaryExpressionNode binary)
        {
            // Generate left operand
            GenerateExpression(binary.Left);
            code.AppendLine("    push rax");

            // Generate right operand
            GenerateExpression(binary.Right);
            code.AppendLine("    mov rbx, rax");
            code.AppendLine("    pop rax");

            // Generate operation
            switch (binary.Operator)
            {
                case TokenType.Plus:
                    code.AppendLine("    add rax, rbx");
                    break;
                case TokenType.Minus:
                    code.AppendLine("    sub rax, rbx");
                    break;
                case TokenType.Multiply:
                    code.AppendLine("    imul rax, rbx");
                    break;
                case TokenType.Divide:
                    code.AppendLine("    cqo");
                    code.AppendLine("    idiv rbx");
                    break;
                case TokenType.Equal:
                    code.AppendLine("    cmp rax, rbx");
                    code.AppendLine("    sete al");
                    code.AppendLine("    movzx rax, al");
                    break;
                default:
                    // Handle other operators
                    break;
            }
        }

        private void GenerateUnaryExpression(UnaryExpressionNode unary)
        {
            GenerateExpression(unary.Operand);

            switch (unary.Operator)
            {
                case TokenType.Minus:
                    code.AppendLine("    neg rax");
                    break;
                case TokenType.Not:
                    code.AppendLine("    cmp rax, 0");
                    code.AppendLine("    sete al");
                    code.AppendLine("    movzx rax, al");
                    break;
            }
        }

        private void GenerateFunctionCall(FunctionCallExpressionNode call)
        {
            // Generate arguments (simplified - assumes function name is identifier)
            if (call.Function is IdentifierExpressionNode funcName)
            {
                // Push arguments in reverse order
                for (int i = call.Arguments.Count - 1; i >= 0; i--)
                {
                    GenerateExpression(call.Arguments[i]);
                    code.AppendLine("    push rax");
                }

                code.AppendLine($"    call {funcName.Name}");

                // Clean up stack
                if (call.Arguments.Count > 0)
                {
                    code.AppendLine($"    add rsp, {call.Arguments.Count * 8}");
                }
            }
        }

        private string GetNextLabel(string prefix)
        {
            return $"{prefix}_{labelCounter++}";
        }

        private int CalculateLocalVariableSpace(BlockStatementNode block)
        {
            // Simplified calculation
            return 64; // Reserve 64 bytes for local variables
        }

        private List<Instruction> ParseInstructions(string code)
        {
            var instructions = new List<Instruction>();
            var lines = code.Split('\n');

            foreach (var line in lines)
            {
                var trimmed = line.Trim();
                if (string.IsNullOrEmpty(trimmed) || trimmed.StartsWith(";"))
                    continue;

                instructions.Add(new Instruction(trimmed));
            }

            return instructions;
        }

        private string GenerateCodeFromInstructions(List<Instruction> instructions)
        {
            var result = new StringBuilder();
            foreach (var instruction in instructions)
            {
                result.AppendLine(instruction.ToString());
            }
            return result.ToString();
        }

        private void CompileAssembly(string asmPath, string outputPath)
        {
            // Simplified assembly compilation
            // In a real implementation, you would use nasm/gas and ld
            Console.WriteLine($"Assembly generated at: {asmPath}");
            Console.WriteLine($"Executable would be generated at: {outputPath}");
        }
    }

    // Instruction representation for optimizations
    public class Instruction
    {
        public string Operation { get; set; }
        public List<string> Operands { get; set; }
        public string Original { get; set; }

        public Instruction(string line)
        {
            Original = line;
            Operands = new List<string>();

            if (line.Contains(':'))
            {
                Operation = "LABEL";
                Operands.Add(line.Trim(':'));
            }
            else
            {
                var parts = line.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                if (parts.Length > 0)
                {
                    Operation = parts[0].ToUpper();
                    for (int i = 1; i < parts.Length; i++)
                    {
                        Operands.Add(parts[i].TrimEnd(','));
                    }
                }
            }
        }

        public override string ToString()
        {
            return Original;
        }
    }

    // Profile data for optimization
    public class ProfileData
    {
        public Dictionary<string, int> FunctionCallCounts { get; set; }
        public Dictionary<string, int> BranchTakenCounts { get; set; }
        public Dictionary<string, int> LoopIterationCounts { get; set; }

        public ProfileData()
        {
            FunctionCallCounts = new Dictionary<string, int>();
            BranchTakenCounts = new Dictionary<string, int>();
            LoopIterationCounts = new Dictionary<string, int>();
        }
    }
}

        private void GenerateFunctionCall(FunctionCallNode functionCall)
        {
            // Resolve the function symbol in the current scope
            var symbol = currentScope.Lookup(functionCall.Name);
            if (symbol == null || !(symbol is FunctionSymbol functionSymbol))
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' not found");
                return;
            }
            // Check if the number of arguments matches the function's parameters
            if (functionCall.Arguments.Count != functionSymbol.Parameters.Count)
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' expects {functionSymbol.Parameters.Count} arguments, but got {functionCall.Arguments.Count}");
                return;
            }
            // Check each argument type against the corresponding parameter type
            for (int i = 0; i < functionCall.Arguments.Count; i++)
            {
                string argType = VisitExpression(functionCall.Arguments[i]);
                string paramType = functionSymbol.Parameters[i].Type;
                if (argType != paramType)
                {
                    errors.Add($"Line {functionCall.Line}: Argument '{i + 1}' type '{argType}' does not match parameter type '{paramType}' in function '{functionCall.Name}'");
                    return;
                }
            }
            // Return the function's return type
            return functionSymbol.ReturnType;
        }

       private void VisitReturnStatement(ReturnStatementNode returnStmt)
        {
            // If the return statement has an expression, visit it
            if (returnStmt.Expression != null)
            {
                string returnType = VisitExpression(returnStmt.Expression);
                // Check if the return type matches the function's expected return type
                if (currentScope.FunctionSymbol != null && currentScope.FunctionSymbol.Type != returnType)
                {
                    errors.Add($"Line {returnStmt.Line}: Function '{currentScope.FunctionSymbol.Name}' expects a return value of type '{currentScope.FunctionSymbol.Type}', got '{returnType}'");
                }
                else if (currentScope.FunctionSymbol == null)
                {
                    errors.Add($"Line {returnStmt.Line}: Return statement outside of function context");
                }
                }
            else
            {
                // If the function expects a return value, report an error
                if (currentScope.FunctionSymbol != null && currentScope.FunctionSymbol.Type != "void")
                {
                    errors.Add($"Line {returnStmt.Line}: Function '{currentScope.FunctionSymbol.Name}' expects a return value of type '{currentScope.FunctionSymbol.Type}', got 'void'");
                }
            }
            private string VisitExpression(ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralNode literal:
                    return VisitLiteral(literal);
                case IdentifierExpressionNode identifier:
                    return currentScope.Lookup(identifier.Name)?.Type ?? "error";
                case BinaryExpressionNode binary:
                    return VisitBinaryExpression(binary);
                case UnaryExpressionNode unary:
                    return VisitUnaryExpression(unary);
                case FunctionCallNode functionCall:
                    return VisitFunctionCall(functionCall);
                case MemberAccessNode memberAccess:
                    return VisitMemberAccess(memberAccess);
                // Add other expression types as needed
                default:
                    errors.Add($"Line {expression.Line}: Unknown expression type '{expression.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitLiteral(LiteralNode literal)
        {
            switch (literal.Value)
            {
                case int _:
                    return "int";
                case float _:
                    return "float";
                case string _:
                    return "string";
                case bool _:
                    return "bool";
                case null:
                    return "null"; // Handle null literals
                default:
                    errors.Add($"Line {literal.Line}: Unknown literal type '{literal.Value.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitBinaryExpression(BinaryExpressionNode binary)
        {
            // Visit left and right expressions
            string leftType = VisitExpression(binary.Left);
            string rightType = VisitExpression(binary.Right);
            
            // Check if either operand type is invalid
            if (leftType == "error" || rightType == "error")
            {
                errors.Add($"Line {binary.Line}: Invalid operand types for binary operation '{binary.Operator}'");
                return "error";
            }
            
            // Check the operator and return the result type
            switch (binary.Operator)
            {
                case TokenType.Plus:
                case TokenType.Minus:
                case TokenType.Multiply:
                case TokenType.Divide:
                    if ((leftType == "int" && rightType == "int") ||
                        (leftType == "float" && rightType == "float") ||
                        (leftType == "int" && rightType == "float") ||
                        (leftType == "float" && rightType == "int"))
                    {
                        return leftType == "float" || rightType == "float" ? "float" : "int";
                    }
                    errors.Add($"Line {binary.Line}: Arithmetic operation '{binary.Operator}' requires numeric operands");
                    return "error";
                case TokenType.Equal:
                case TokenType.NotEqual:
                    if (leftType == rightType)
                        return "bool";
                    errors.Add($"Line {binary.Line}: Comparison operation '{binary.Operator}' requires operands of the same type");
                    return "error";
                case TokenType.LessThan:
                case TokenType.GreaterThan:
                case TokenType.LessThanOrEqual:
                case TokenType.GreaterThanOrEqual:
                    if ((leftType == "int" && rightType == "int") ||
                        (leftType == "float" && rightType == "float") ||
                        (leftType == "int" && rightType == "float") ||
                        (leftType == "float" && rightType == "int"))
                    {
                        return "bool";
                    }
                    errors.Add($"Line {binary.Line}: Relational operation '{binary.Operator}' requires numeric operands");
                    return "error";
                case TokenType.And:
                case TokenType.Or:
                    if (leftType == "bool" && rightType == "bool")
                        return "bool";
                    errors.Add($"Line {binary.Line}: Logical operation '{binary.Operator}' requires boolean operands");
                    return "error";
                default:
                    errors.Add($"Line {binary.Line}: Unknown binary operator '{binary.Operator}'");
                    return "error";
            }
        }
        private string VisitUnaryExpression(UnaryExpressionNode unary)
        {
            // Visit the operand expression
            string operandType = VisitExpression(unary.Operand);
            
            // Check if the operand type is invalid
            if (operandType == "error")
            {
                errors.Add($"Line {unary.Line}: Invalid operand type for unary operation '{unary.Operator}'");
                return "error";
            }
            
            // Check the operator and return the result type
            switch (unary.Operator)
            {
                case TokenType.Not:
                    if (operandType == "bool")
                        return "bool";
                    errors.Add($"Line {unary.Line}: '!' operator requires boolean operand, got '{operandType}'");
                    return "error";
                case TokenType.Minus:
                    if (operandType == "int" || operandType == "float")
                        return operandType; // Return the same numeric type
                    errors.Add($"Line {unary.Line}: '-' operator requires numeric operand, got '{operandType}'");
                    return "error";
                default:
                    errors.Add($"Line {unary.Line}: Unknown unary operator '{unary.Operator}'");
                    return "error";
            }
        }
        private string VisitFunctionCall(FunctionCallNode functionCall)
        {
            // Resolve the function symbol in the current scope
            var symbol = currentScope.Lookup(functionCall.Name);
            if (symbol == null || !(symbol is FunctionSymbol functionSymbol))
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' not found");
                return "error";
            }
            // Check if the number of arguments matches the function's parameters
            if (functionCall.Arguments.Count != functionSymbol.Parameters.Count)
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' expects {functionSymbol.Parameters.Count} arguments, but got {functionCall.Arguments.Count}");
                return "error";
            }
            // Check each argument type against the corresponding parameter type
            for (int i = 0; i < functionCall.Arguments.Count; i++)
            {
                string argType = VisitExpression(functionCall.Arguments[i]);
                string paramType = functionSymbol.Parameters[i].Type;
                if (argType != paramType)
                {
                    errors.Add($"Line {functionCall.Line}: Argument '{i + 1}' type '{argType}' does not match parameter type '{paramType}' in function '{functionCall.Name}'");
                    return "error";
                }
            }
            // Return the function's return type
            return functionSymbol.ReturnType;
        }
        private string VisitMemberAccess(MemberAccessNode memberAccess)
        {
            // Resolve the object type
            string objectType = VisitExpression(memberAccess.Object);
            if (objectType == "error")
            {
                errors.Add($"Line {memberAccess.Line}: Invalid object type for member access '{memberAccess.MemberName}'");
                return "error";
            }
            
            // Check if the member exists in the object type
            var memberSymbol = currentScope.LookupMember(objectType, memberAccess.MemberName);
            if (memberSymbol == null)
            {
                errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in type '{objectType}'");
                return "error";
            }
            
            // Return the member's type
            return memberSymbol.Type;
        }
        private bool IsAssignableType(string targetType, string sourceType)
        {
            // Check if the source type can be assigned to the target type
            if (targetType == sourceType)
                return true;
            // Handle numeric promotions
            if ((targetType == "float" && sourceType == "int") ||
                (targetType == "int" && sourceType == "float"))
                return true;
            // Handle other cases as needed
            return false;
        }
        private string GetTypeName(TypeNode typeNode)
        {
            if (typeNode is BuiltinTypeNode builtin)
            {
                return builtin.Name; // Return built-in type name
            }
            else if (typeNode is IdentifierTypeNode identifier)
            {
                return identifier.Name; // Return identifier type name
            }
            else if (typeNode is CapsuleTypeNode capsule)
            {
                return $"capsule.{capsule.Name}"; // Return capsule type
            }
            else if (typeNode is Struc
        tTypeNode struct)
            {
                // Return struct type with its name
                return $"struct.{struct.Name}"; // Return struct type
            }
            else if (typeNode is EnumTypeNode enumType)
            {
                // Return enum type with its name
                return $"en
um.{enumType.Name}"; // Return enum type
            }
            else
            {
                errors.Add($"Line {typeNode.Line}: Unknown type node '{typeNode.GetType().Name}'");
                return "error";
            }
        }
        private string VisitMemberAccess(MemberAccessNode memberAccess)
        {
            // Resolve the object type
            string objectType = VisitExpression(memberAccess.Object);
            if (objectType == "error")
            {
                errors.Add($"Line {memberAccess.Line}: Invalid object type for member access '{memberAccess.MemberName}'");
                return "error";
            }
            
            // Check if the member exists in the object type
            var memberSymbol = currentScope.LookupMember(objectType, memberAccess.MemberName);
            if (memberSymbol == null)
            {
                errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in type '{objectType}'");
                return "error";
            }

            // Return the member's type
            return memberSymbol.Type;
        }
        private void VisitDeclaration(DeclarationNode declaration)
        {
            switch (declaration)
            {
                case FunctionDeclarationNode functionDecl:
                    VisitFunctionDeclaration(functionDecl);
                    break;
                case VariableDeclarationNode variableDecl:
                    VisitVariableDeclaration(variableDecl);
                    break;
                case CapsuleDeclarationNode capsuleDecl:
                    VisitCapsuleDeclaration(capsuleDecl);
                    break;
                case StructDeclarationNode structDecl:
                    VisitStructDeclaration(structDecl);
                    break;
                case EnumDeclarationNode enumDecl:
                    VisitEnumDeclaration(enumDecl);
                    break;
                default:
                    errors.Add($"Line {declaration.Line}: Unknown declaration type '{declaration.GetType().Name}'");
                    break;
            }
        }
        private void VisitFunctionDeclaration(FunctionDeclarationNode functionDecl)
        {
            // Check if the function is already defined
            if (currentScope.Lookup(functionDecl.Name) != null)
            {
                errors.Add($"Line {functionDecl.Line}: Function '{functionDecl.Name}' is already defined");
                return;
            }
            // Create a new function symbol and add it to the current scope
            var functionSymbol = new FunctionSymbol(functionDecl.Name, functionDecl.ReturnType, functionDecl.Parameters);
            currentScope.AddSymbol(functionSymbol);
            // Visit the function body
            if (functionDecl.Body != null)
            {
                VisitBlockStatement(functionDecl.Body);
            }
        }
        private void VisitVariableDeclaration(VariableDeclarationNode variableDecl)
        {
            // Check if the variable is already defined
            if (currentScope.Lookup(variableDecl.Name) != null)
            {
                errors.Add($"Line {variableDecl.Line}: Variable '{variableDecl.Name}' is already defined");
                return;
            }
            // Create a new variable symbol and add it to the current scope
            var variableSymbol = new VariableSymbol(variableDecl.Name, variableDecl.Type, variableDecl.Initializer);
            currentScope.AddSymbol(variableSymbol);
        }
        private void VisitCapsuleDeclaration(CapsuleDeclarationNode capsuleDecl)
        {
            // Check if the capsule is already defined
            if (currentScope.Lookup(capsuleDecl.Name) != null)
            {
                errors.Add($"Line {capsuleDecl.Line}: Capsule '{capsuleDecl.Name}' is already defined");
                return;
            }
            // Create a new capsule symbol and add it to the current scope
            var capsuleSymbol = new CapsuleSymbol(capsuleDecl.Name, capsuleDecl.Members);
            currentScope.AddSymbol(capsuleSymbol);
        }
        private void VisitStructDeclaration(StructDeclarationNode structDecl)
        {
            // Check if the struct is already defined
            if (currentScope.Lookup(structDecl.Name) != null)
            {
                errors.Add($"Line {structDecl.Line}: Struct '{structDecl.Name}' is already defined");
                return;
            }
            // Create a new struct symbol and add it to the current scope
            var structSymbol = new StructSymbol(structDecl.Name, structDecl.Members);
            currentScope.AddSymbol(structSymbol);
        }
        private void VisitEnumDeclaration(EnumDeclarationNode enumDecl)
        {
            // Check if the enum is already defined
            if (currentScope.Lookup(enumDecl.Name) != null)
            {
                errors.Add($"Line {enumDecl.Line}: Enum '{enumDecl.Name}' is already defined");
                return;
            }
            // Create a new enum symbol and add it to the current scope
            var enumSymbol = new EnumSymbol(enumDecl.Name, enumDecl.Members);
            currentScope.AddSymbol(enumSymbol);
        }
using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using PhoenixCompiler.AST;
using PhoenixCompiler.AST.Nodes;
using PhoenixCompiler.AST.Nodes.Expressions;
using PhoenixCompiler.AST.Nodes.Statements;
using PhoenixCompiler.AST.Nodes.Declarations;
using PhoenixCompiler.AST.Nodes.Types;
using PhoenixCompiler.AST.Nodes.Members;
using PhoenixCompiler.AST.Nodes.Functions;
using PhoenixCompiler.AST.Nodes.Literals;
using PhoenixCompiler.AST.Nodes.Identifiers;
using PhoenixCompiler.AST.Nodes.Unary;
using PhoenixCompiler.AST.Nodes.Binary;
using PhoenixCompiler.AST.Nodes.Capsules;
using PhoenixCompiler.AST.Nodes.Structs;
using PhoenixCompiler.AST.Nodes.Enums;
using PhoenixCompiler.AST.Nodes.Conditions;
using PhoenixCompiler.AST.Nodes.Loops;
using PhoenixCompiler.AST.Nodes.Returns;
namespace PhoenixCompiler
{
    public class CodeGenerator
    {
        private StringBuilder code;
        private int labelCounter;
        private Dictionary<string, int> variableOffsets;
        private int stackOffset;
        public CodeGenerator()
        {
            code = new StringBuilder();
            labelCounter = 0;
            variableOffsets = new Dictionary<string, int>();
            stackOffset = 0;
        }
        public string GenerateCode(FunctionNode function)
        {
            code.Clear();
            labelCounter = 0;
            variableOffsets.Clear();
            stackOffset = 0;
            GenerateFunctionHeader(function);
            GenerateFunctionBody(function);
            return code.ToString();
        }
        private void GenerateFunctionHeader(FunctionNode function)
        {
            code.AppendLine($"; Function: {function.Name}");
            code.AppendLine($"{function.Name}:");
            code.AppendLine("    push rbp");
            code.AppendLine("    mov rbp, rsp");
        }
        private void GenerateFunctionBody(FunctionNode function)
        {
            // Calculate local variable space
            int localVarSpace = CalculateLocalVariableSpace(function.Body);
            if (localVarSpace > 0)
            {
                code.AppendLine($"    sub rsp, {localVarSpace}");
                stackOffset += localVarSpace;
            }
            // Generate variable declarations
            foreach (var declaration in function.Body.Declarations)
            {
                GenerateDeclaration(declaration);
            }
            // Generate statements
            foreach (var statement in function.Body.Statements)
            {
                GenerateStatement(statement);
            }
            // Restore stack and return
            code.AppendLine("    mov rsp, rbp");
            code.AppendLine("    pop rbp");
            code.AppendLine("    ret");
        }
        private void GenerateDeclaration(DeclarationNode declaration)
        {
            switch (declaration)
            {
                case VariableDeclarationNode varDecl:
                    GenerateVariableDeclaration(varDecl);
                    break;
                case FunctionDeclarationNode funcDecl:
                    GenerateFunctionDeclaration(funcDecl);
                    break;
                // Handle other declarations as needed
                default:
                    code.AppendLine($"    ; Unknown declaration: {declaration.GetType().Name}");
                    break;
            }
        }
        private void GenerateVariableDeclaration(VariableDeclarationNode varDecl)
        {
            // Allocate space for the variable
            int offset = stackOffset;
            variableOffsets[varDecl.Name] = offset;
            stackOffset += 8; // Assuming 64-bit integers
            code.AppendLine($"    sub rsp, 8");
            code.AppendLine($"    mov [rbp-{offset}], 0"); // Initialize to 0
        }
        private void GenerateFunctionDeclaration(FunctionDeclarationNode funcDecl)
        {
            // Generate function header
            code.AppendLine($"; Function: {funcDecl.Name}");
            code.AppendLine($"{funcDecl.Name}:");
            code.AppendLine("    push rbp");
            code.AppendLine("    mov rbp, rsp");
            // Generate local variable space
            int localVarSpace = CalculateLocalVariableSpace(funcDecl.Body);
            if (localVarSpace > 0)
            {
                code.AppendLine($"    sub rsp, {localVarSpace}");
                stackOffset += localVarSpace;
            }
            // Generate function body
            foreach (var statement in funcDecl.Body.Statements)
            {
                GenerateStatement(statement);
            }
            // Restore stack and return
            code.AppendLine("    mov rsp, rbp");
            code.AppendLine("    pop rbp");
            code.AppendLine("    ret");
        }
        private void GenerateStatement(StatementNode statement)
        {
            switch (statement)
            {
                case ExpressionStatementNode exprStmt:
                    GenerateExpression(exprStmt.Expression);
                    break;
                case ReturnStatementNode returnStmt:
                    GenerateReturnStatement(returnStmt);
                    break;
                case IfStatementNode ifStmt:
                    GenerateIfStatement(ifStmt);
                    break;
                case WhileStatementNode whileStmt:
                    GenerateWhileStatement(whileStmt);
                    break;
                // Handle other statements as needed
                default:
                    code.AppendLine($"    ; Unknown statement: {statement.GetType().Name}");
                    break;
            }
        }
        private void GenerateReturnStatement(ReturnStatementNode returnStmt)
        {
            if (returnStmt.Expression != null)
            {
                GenerateExpression(returnStmt.Expression);
                code.AppendLine("    mov rax, rax"); // Move return value to rax
            }
            code.AppendLine("    leave");
            code.AppendLine("    ret");
        }
        private void GenerateIfStatement(IfStatementNode ifStmt)
        {
            string endLabel = GetNextLabel("if_end");
            GenerateExpression(ifStmt.Condition);
            code.AppendLine("    cmp rax, 0");
            code.AppendLine($"    je {endLabel}");
            foreach (var statement in ifStmt.Body.Statements)
            {
                GenerateStatement(statement);
            }
            code.AppendLine($"{endLabel}:");
        }
        private void GenerateWhileStatement(WhileStatementNode whileStmt)
        {
            string startLabel = GetNextLabel("while_start");
            string endLabel = GetNextLabel("while_end");
            code.AppendLine($"{startLabel}:");
            GenerateExpression(whileStmt.Condition);
            code.AppendLine("    cmp rax, 0");
            code.AppendLine($"    je {endLabel}");
            foreach (var statement in whileStmt.Body.Statements)
            {
                GenerateStatement(statement);
            }
            code.AppendLine($"    jmp {startLabel}");
            code.AppendLine($"{endLabel}:");
        }
        private void GenerateExpression(ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralNode literal:
                    GenerateLiteral(literal);
                    break;
                case IdentifierExpressionNode identifier:
                    GenerateIdentifier(identifier);
                    break;
                case BinaryExpressionNode binary:
                    GenerateBinaryExpression(binary);
                    break;
                case UnaryExpressionNode unary:
                    GenerateUnaryExpression(unary);
                    break;
                case FunctionCallExpressionNode functionCall:
                    GenerateFunctionCall(functionCall);
                    break;
                // Handle other expression types as needed
                default:
                    code.AppendLine($"    ; Unknown expression: {expression.GetType().Name}");
                    break;
            }
        }
        private void GenerateLiteral(LiteralNode literal)
        {
            switch (literal.Value)
            {
                case int intValue:
                    code.AppendLine($"    mov rax, {intValue}");
                    break;
                case float floatValue:
                    code.AppendLine($"    mov rax, {floatValue}");
                    break;
                case string strValue:
                    // Handle string literals (simplified)
                    code.AppendLine($"    mov rax, \"{strValue}\"");
                    break;
                case bool boolValue:
                    code.AppendLine($"    mov rax, {(boolValue ? 1 : 0)}");
                    break;
                case null:
                    code.AppendLine("    mov rax, 0"); // Handle null
                    break;
                default:
                    code.AppendLine($"    ; Unknown literal type: {literal.Value.GetType().Name}");
                    break;
            }
        }
        private void GenerateIdentifier(IdentifierExpressionNode identifier)
        {
            if (variableOffsets.TryGetValue(identifier.Name, out int offset))
            {
                code.AppendLine($"    mov rax, [rbp-{offset}]"); // Load variable value
            }
            else
            {
                code.AppendLine($"    ; Undefined variable: {identifier.Name}");
            }
        }
        private void GenerateBinaryExpression(BinaryExpressionNode binary)
        {
            GenerateExpression(binary.Left);
            code.AppendLine("    push rax"); // Save left operand
            GenerateExpression(binary.Right);
            code.AppendLine("    pop rbx"); // Load left operand into rbx
            
            switch (binary.Operator)
            {
                case TokenType.Plus:
                    code.AppendLine("    add rax, rbx");
                    break;
                case TokenType.Minus:
                    code.AppendLine("    sub rax, rbx");
                    break;
                case TokenType.Multiply:
                    code.AppendLine("    imul rax, rbx");
                    break;
                case TokenType.Divide:
                    code.AppendLine("    xor rdx, rdx"); // Clear remainder
                    code.AppendLine("    idiv rbx");
                    break;
                case TokenType.Equal:
                    code.AppendLine("    cmp rax, rbx");
                    code.AppendLine("    sete al"); // Set al to 1 if equal, 0 otherwise
                    break;
                case TokenType.NotEqual:
                    code.AppendLine("    cmp rax, rbx");
                    code.AppendLine("    setne al"); // Set al to 1 if not equal, 0 otherwise
                    break;
                case TokenType.LessThan:
                    code.AppendLine("    cmp rax, rbx");
                    code.AppendLine("    setl al"); // Set al to 1 if less than, 0 otherwise
                    break;
                case TokenType.GreaterThan:
                    code.AppendLine("    cmp rax, rbx");
                    code.AppendLine("    setg al"); // Set al to 1 if greater than, 0 otherwise
                    break;
                case TokenType.LessThanOrEqual:
                    code.AppendLine("    cmp rax, rbx");
                    code.AppendLine("    setle al"); // Set al to 1 if less than or equal, 0 otherwise
                    break;
                case TokenType.GreaterThanOrEqual:
                    code.AppendLine("    cmp rax, rbx");
                    code.AppendLine("    setge al"); // Set al to 1 if greater than or equal, 0 otherwise
                    break;
                default:
                    code.AppendLine($"    ; Unknown binary operator: {binary.Operator}");
                    break;
            }
        }
        private void GenerateUnaryExpression(UnaryExpressionNode unary)
        {
            GenerateExpression(unary.Operand);
            switch (unary.Operator)
            {
                case TokenType.Not:
                    code.AppendLine("    xor rax, 1"); // Logical NOT
                    break;
                case TokenType.Minus:
                    code.AppendLine("    neg rax"); // Negation
                    break;
                default:
                    code.AppendLine($"    ; Unknown unary operator: {unary.Operator}");
                    break;
            }
        }
        private void GenerateFunctionCall(FunctionCallExpressionNode functionCall)
        {
            // Generate code for each argument
            foreach (var arg in functionCall.Arguments)
            {
                GenerateExpression(arg);
                code.AppendLine("    push rax"); // Push argument onto stack
            }
            // Call the function
            code.AppendLine($"    call {functionCall.Name}");
            // Clean up the stack after the call
            code.AppendLine($"    add rsp, {functionCall.Arguments.Count * 8}"); // Assuming 64-bit integers
        }
        private string GetNextLabel(string prefix)
        {
            return $"{prefix}_{labelCounter++}";
        }
        private int CalculateLocalVariableSpace(BlockNode block)
        {
            int space = 0;
            foreach (var declaration in block.Declarations)
            {
                if (declaration is VariableDeclarationNode varDecl)
                {
                    space += 8; // Assuming 64-bit integers
                }
            }
            return space;
        }
        private string VisitExpression(ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralNode literal:
                    return VisitLiteral(literal);
                case IdentifierExpressionNode identifier:
                    return currentScope.Lookup(identifier.Name)?.Type ?? "error";
                case BinaryExpressionNode binary:
                    return VisitBinaryExpression(binary);
                case UnaryExpressionNode unary:
                    return VisitUnaryExpression(unary);
                case FunctionCallNode functionCall:
                    return VisitFunctionCall(functionCall);
                case MemberAccessNode memberAccess:
                    return VisitMemberAccess(memberAccess);
                // Add other expression types as needed
                default:
                    errors.Add($"Line {expression.Line}: Unknown expression type '{expression.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitLiteral(LiteralNode literal)
        {
            switch (literal.Value)
            {
                case int _:
                    return "int";
                case float _:
                    return "float";
                case string _:
                    return "string";
                case bool _:
                    return "bool";
                case null:
                    return "null"; // Handle null literals
                default:
                    errors.Add($"Line {literal.Line}: Unknown literal type '{literal.Value.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitBinaryExpression(BinaryExpressionNode binary)
        {
            // Visit left and right expressions
            string leftType = VisitExpression(binary.Left);
            string rightType = VisitExpression(binary.Right);
            
            // Check if either operand type is invalid
            if (leftType == "error" || rightType == "error")
            {
                errors.Add($"Line {binary.Line}: Invalid operand types for binary operation '{binary.Operator}'");
                return "error";
            }
            
            // Check the operator and return the result type
            switch (binary.Operator)
            {
                case TokenType.Plus:
                case TokenType.Minus:
                case TokenType.Multiply:
                case TokenType.Divide:
                    if ((leftType == "int" && rightType == "int") ||
                        (leftType == "float" && rightType == "float") ||
                        (leftType == "int" && rightType == "float") ||
                        (leftType == "float" && rightType == "int"))
                    {
                        return leftType == "float" || rightType == "float" ? "float" : "int";
                    }
                    errors.Add($"Line {binary.Line}: Arithmetic operation '{binary.Operator}' requires numeric operands");
                    return "error";
                
                case TokenType.Equal:
                case TokenType.NotEqual:
                    if (leftType == rightType)
                        return "bool";
                    errors.Add($"Line {binary.Line}: Comparison operation '{binary.Operator}' requires operands of the same type");
                    return "error";
                
                case TokenType.LessThan:
                case TokenType.GreaterThan:
                case TokenType.LessThanOrEqual:
                case TokenType.GreaterThanOrEqual:
                    if ((leftType == "int" && rightType == "int") ||
                        (leftType == "float" && rightType == "float") ||
                        (leftType == "int" && rightType == "float") ||
                        (leftType == "float" && rightType == "int"))
                    {
                        return "bool";
                    }
                    errors.Add($"Line {binary.Line}: Relational operation '{binary.Operator}' requires numeric operands");
                    return "error";
                
                case TokenType.And:
                case TokenType.Or:
                    if (leftType == "bool" && rightType == "bool")
                        return "bool";
                    errors.Add($"Line {binary.Line}: Logical operation '{binary.Operator}' requires
                        boolean operands");
                        return "error";
                    default:
                    errors.Add($"Line {binary.Line}: Unknown binary operator '{binary.Operator}'");
                    return "error";
                    }
            }
        private string VisitUnaryExpression(UnaryExpressionNode unary)
            {
            // Visit the operand expression
            string operandType = VisitExpression(unary.Operand);
            
            // Check if the operand type is invalid
            if (operandType == "error")
            {
                errors.Add($"Line {unary.Line}: Invalid operand type for unary operation '{unary.Operator}'");
                return "error";
            }
            
            // Check the operator and return the result type
            switch (unary.Operator)
            {
                case TokenType.Not:
                    if (operandType == "bool")
                        return "bool";
                    errors.Add($"Line {unary.Line}: '!' operator requires boolean operand, got '{operandType}'");
                    return "error";
                case TokenType.Minus:
                    if (operandType == "int" || operandType == "float")
                        return operandType; // Return the same numeric type
                    errors.Add($"Line {unary.Line}: '-' operator requires numeric operand, got '{operandType}'");
                    return "error";
                default:
                    errors.Add($"Line {unary.Line}: Unknown unary operator '{unary.Operator}'");
                    return "error";
            }
        }
        private string VisitFunctionCall(FunctionCallNode functionCall)
        {
            // Resolve the function symbol in the current scope
            var symbol = currentScope.Lookup(functionCall.Name);
            if (symbol == null || !(symbol is FunctionSymbol functionSymbol))
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' not found");
                return "error";
            }
            // Check if the number of arguments matches the function's parameters
            if (functionCall.Arguments.Count != functionSymbol.Parameters.Count)
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' expects {functionSymbol.Parameters.Count} arguments, but got {functionCall.Arguments.Count}");
                return "error";
            }
            // Check each argument type against the corresponding parameter type
            for (int i = 0; i < functionCall.Arguments.Count; i++)
            {
                string argType = VisitExpression(functionCall.Arguments[i]);
                string paramType = functionSymbol.Parameters[i].Type;
                if (argType != paramType)
                {
                    errors.Add($"Line {functionCall.Line}: Argument '{i + 1}' type '{argType}' does not match parameter type '{paramType}' in function '{functionCall.Name}'");
                    return "error";
                }
            }
            // Return the function's return type
            return functionSymbol.ReturnType;
        }
        private string VisitMemberAccess(MemberAccessNode memberAccess)
        {
            // Resolve the object type
            string objectType = VisitExpression(memberAccess.Object);
            if (objectType == "error")
            {
                errors.Add($"Line {memberAccess.Line}: Invalid object type for member access '{memberAccess.MemberName}'");
                return "error";
            }
            
            // Check if the member exists in the object type
            var memberSymbol = currentScope.LookupMember(objectType, memberAccess.MemberName);
            if (memberSymbol == null)
            {
                errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in type '{objectType}'");
                return "error";
            }
            
            // Return the member's type
            return memberSymbol.Type;
        }
        private string VisitUnaryExpression(UnaryExpressionNode unary)
        {
            // Visit the operand expression
            string operandType = VisitExpression(unary.Operand);
            
            // Check if the operand type is invalid
            if (operandType == "error")
            {
                errors.Add($"Line {unary.Line}: Invalid operand type for unary operation '{unary.Operator}'");
                return "error";
            }
            
            // Check the operator and return the result type
            switch (unary.Operator)
            {
                case TokenType.Not:
                    if (operandType == "bool")
                        return "bool";
                    errors.Add($"Line {unary.Line}: '!' operator requires boolean operand, got '{operandType}'");
                    return "error";
                    case TokenType.Minus:
                    if (operandType == "int" || operandType == "float")
                        return operandType; // Return the same numeric type
                    errors.Add($"Line {unary.Line}: '-' operator requires numeric operand, got '{operandType}'");
                    return "error";
                    default:
                    errors.Add($"Line {unary.Line}: Unknown unary operator '{unary.Operator}'");
                    return "error";
                    }
            }
        private string VisitMemberAccess(MemberAccessNode memberAccess)
            {
            // Resolve the object type
            string objectType = VisitExpression(memberAccess.Object);
            if (objectType == "error")
            {
                errors.Add($"Line {memberAccess.Line}: Invalid object type for member access '{memberAccess.MemberName}'");
                return "error";
            }
            
            // Check if the member exists in the object type
            var memberSymbol = currentScope.LookupMember(objectType, memberAccess.MemberName);
            if (memberSymbol == null)
            {
                errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in type '{obje
                    ctType}'");
                return "error";
                }
            // Return the member's type
            return memberSymbol.Type;
            }
        private string GetTypeName(TypeNode typeNode)
            {
            if (typeNode is IdentifierTypeNode identifier)
            {
                return identifier.Name; // Return identifier type name
            }
            else if (typeNode is CapsuleTypeNode capsule)
            {
                return $"capsule.{capsule.Name}"; // Return capsule type
            }
            else if (typeNode is StructTypeNode struct)
            {
                return $"struct.{struct.Name}"; // Return struct type
            }
            else if (typeNode is EnumTypeNode enumType)
            {
                return $"enum.{enumType.Name}"; // Return enum type
            }
            else
            {
                errors.Add($"Line {typeNode.Line}: Unknown type node '{typeNode.GetType().Name}'");
                return "error";
            }
        }
            if (memberAccess.Object is IdentifierExpressionNode objectId)
            {
                // Lookup the member in the current scope
                var memberSymbol = currentScope.LookupMember(objectId.Name, memberAccess.MemberName);
                if (memberSymbol == null)
                {
                    errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in '{objectId.Name}'");
                    return "error";
                }
                return memberSymbol.Type;
            }
            else
            {
                errors.Add($"Line {memberAccess.Line}: Invalid object type for member access '{memberAccess.Objec
                    t.GetType().Name}'");
                return "error";
            }
            private string VisitExpression(ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralNode literal:
                    return VisitLiteral(literal);
                case IdentifierExpressionNode identifier:
                    return currentScope.Lookup(identifier.Name)?.Type ?? "error";
                case BinaryExpressionNode binary:
                    return VisitBinaryExpression(binary);
                case UnaryExpressionNode unary:
                    return VisitUnaryExpression(unary);
                case FunctionCallExpressionNode functionCall:
                    return VisitFunctionCall(functionCall);
                case MemberAccessExpressionNode memberAccess:
                    return VisitMemberAccess(memberAccess);
                // Add other expression types as needed
                default:
                    errors.Add($"Line {expression.Line}: Unknown expression type '{expression.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitLiteral(LiteralNode literal)
        {
            switch (literal.Value)
            {
                case int _:
                    return "int";
                case float _:
                    return "float";
                case string _:
                    return "string";
                case bool _:
                    return "bool";
                case null:
                    return "null"; // Handle null literals
                default:
                    errors.Add($"Line {literal.Line}: Unknown literal type '{literal.Value.GetType().Name}'");
                    return "error";
            }
        }
        private string VisitBinaryExpression(BinaryExpressionNode binary)
        {
            // Visit left and right expressions
            string leftType = VisitExpression(binary.Left);
            string rightType = VisitExpression(binary.Right);
            
            // Check if either operand type is invalid
            if (leftType == "error" || rightType == "error")
            {
                errors.Add($"Line {binary.Line}: Invalid operand types for binary operation '{binary.Operator}'");
                return "error";
            }
            
            // Check the operator and return the result type
            switch (binary.Operator)
            {
                case TokenType.Plus:
                case TokenType.Minus:
                case TokenType.Multiply:
                case TokenType.Divide:
                    if ((leftType == "int" && rightType == "int") ||
                        (leftType == "float" && rightType == "float") ||
                        (leftType == "int" && rightType == "float") ||
                        (leftType == "float" && rightType == "int"))
                    {
                        return leftType == "float" || rightType == "float" ? "float" : "int";
                    }
                    errors.Add($"Line {binary.Line}: Arithmetic operation '{binary.Operator}' requires numeric operands");
                    return "error";
                
                case TokenType.Equal:
                case TokenType.NotEqual:
                    if (leftType == rightType)
                        return "bool";
                    errors.Add($"Line {binary.Line}: Comparison operation '{binary.Operator}' requires operands of the same type");
                    return "error";
                
                case TokenType.LessThan:
                case TokenType.GreaterThan:
                case TokenType.LessThanOrEqual:
                case TokenType.GreaterThanOrEqual:
                    if ((leftType == "int" && rightType == "int") ||
                        (leftType == "float" && rightType == "float") ||
                        (leftType == "int" && rightType == "float") ||
                        (leftType == "float" && rightType == "int"))
                    {
                        return "bool";
                    }
                    errors.Add($"Line {binary.Line}: Relational operation '{binary.Operator}' requires numeric operands");
                    return "error";
                
                case TokenType.And:
                case TokenType.Or:
                    if (leftType == "bool" && rightType == "bool")
                        return "bool";
                    errors.Add($"Line {binary.Line}: Logical operation '{binary.Operator}' requires
                        boolean operands");
                        return "error";
                default:
                    errors.Add($"Line {binary.Line}: Unknown binary operator '{binary.Operator}'");
                    return "error";
            }
        }
        private string VisitUnaryExpression(UnaryExpressionNode unary)
        {
            // Visit the operand expression
            string operandType = VisitExpression(unary.Operand);
            
            // Check if the operand type is invalid
            if (operandType == "error")
            {
                errors.Add($"Line {unary.Line}: Invalid operand type for unary operation '{unary.Operator}'");
                return "error";
            }
            
            // Check the operator and return the result type
            switch (unary.Operator)
            {
                case TokenType.Not:
                    if (operandType == "bool")
                        return "bool";
                    errors.Add($"Line {unary.Line}: '!' operator requires boolean operand, got '{operandType}'");
                    return "error";
                case TokenType.Minus:
                    if (operandType == "int" || operandType == "float")
                        return operandType; // Return the same numeric type
                    errors.Add($"Line {unary.Line}: '-' operator requires numeric operand, got '{operandType}'");
                    return "error";
                default:
                    errors.Add($"Line {unary.Line}: Unknown unary operator '{unary.Operator}'");
                    return "error";
            }
}
        private string VisitFunctionCall(FunctionCallNode functionCall)
        {
            // Resolve the function symbol in the current scope
            var symbol = currentScope.Lookup(functionCall.Name);
            if (symbol == null || !(symbol is FunctionSymbol functionSymbol))
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' not found");
                return "error";
            }
            // Check if the number of arguments matches the function's parameters
            if (functionCall.Arguments.Count != functionSymbol.Parameters.Count)
            {
                errors.Add($"Line {functionCall.Line}: Function '{functionCall.Name}' expects {functionSymbol.Parameters.Count} arguments, but got {functionCall.Arguments.Count}");
                return "error";
            }
            // Check each argument type against the corresponding parameter type
            for (int i = 0; i < functionCall.Arguments.Count; i++)
            {
                string argType = VisitExpression(functionCall.Arguments[i]);
                string paramType = functionSymbol.Parameters[i].Type;
                if (argType != paramType)
                {
                    errors.Add($"Line {functionCall.Line}: Argument '{i + 1}' type '{argType}' does not match parameter type '{paramType}' in function '{functionCall.Name}'");
                    return "error";
                }
            }
            // Return the function's return type
            return functionSymbol.ReturnType;
}
        private string VisitMemberAccess(MemberAccessNode memberAccess)
        {
            // Resolve the object type
            string objectType = VisitExpression(memberAccess.Object);
            if (objectType == "error")
            {
                errors.Add($"Line {memberAccess.Line}: Invalid object type for member access '{memberAccess.MemberName}'");
                return "error";
            }
            
            // Check if the member exists in the object type
            var memberSymbol = currentScope.LookupMember(objectType, memberAccess.MemberName);
            if (memberSymbol == null)
            {
                errors.Add($"Line {memberAccess.Line}: Member '{memberAccess.MemberName}' not found in type '{objectType}'");
                return "error";
            }
            
            // Return the member's type
            return memberSymbol.Type;
        }
        
        private string GetTypeName(TypeNode typeNode)
        {
            if (typeNode is IdentifierTypeNode identifier)
            {
                return identifier.Name; // Return identifier type name
            }
            else if (typeNode is CapsuleTypeNode capsule)
            {
                return $"capsule.{capsule.Name}"; // Return capsule type
            }
            else if (typeNode is StructTypeNode struct)
            {
                return $"struct.{struct.Name}"; // Return struct type
            }
            else if (typeNode is EnumTypeNode enumType)
            {
                return $"enum.{enumType.Name}"; // Return enum type
            }
            else
            {
                errors.Add($"Line {typeNode.Line}: Unknown type node '{typeNode.GetType().Name}'");
                return "error";
}
        }
    }
}
            // SemanticAnalyzer.cs
            using System;
            using System.Collections.Generic;
            using System.Text;
            namespace SemanticAnalyzer
{
    public class SemanticAnalyzer
    {
        private readonly List<string> errors = new List<string>();
        private Scope currentScope;
        private int labelCounter = 0;
        private StringBuilder code = new StringBuilder();
        private Dictionary<string, int> variableOffsets = new Dictionary<string, int>();
        public SemanticAnalyzer(Scope initialScope)
        {
            currentScope = initialScope;
        }
        public List<string> Analyze(BlockNode block)
        {
            VisitBlock(block);
            return errors;
        }
        private void VisitBlock(BlockNode block)
        {
            // Process variable declarations
            foreach (var declaration in block.Declarations)
            {
                if (declaration is VariableDeclarationNode varDecl)
                {
                    string typeName = GetTypeName(varDecl.Type);
                    if (typeName == "error")
                    {
                        errors.Add($"Line {varDecl.Line}: Invalid type '{varDecl.Type}' for variable '{varDecl.Name}'");
                        continue;
                    }
                    if (currentScope.Lookup(varDecl.Name) != null)
                    {
                        errors.Add($"Line {varDecl.Line}: Variable '{varDecl.Name}' already declared in this scope");
                        continue;
                    }
                    currentScope.Define(new VariableSymbol(varDecl.Name, typeName, varDecl.Line));
                    variableOffsets[varDecl.Name] = CalculateLocalVariableSpace(block);
                }
            }
            // Process statements
            foreach (var statement in block.Statements)
            {
                VisitStatement(statement);
            }
        }
        private void VisitStatement(StatementNode statement)
        {
            switch (statement)
            {
                case ReturnStatementNode returnStmt:
                    GenerateReturnStatement(returnStmt);
                    break;
                case IfStatementNode ifStmt:
                    GenerateIfStatement(ifStmt);
                    break;
                    case WhileStatementNode whileStmt:
                    GenerateWhileStatement(whileStmt);
                    break;
                    case ForStatementNode forStmt:
                    GenerateForStatement(forStmt);
                    break;
                    case ExpressionStatementNode exprStmt:
                    GenerateExpressionStatement(exprStmt);
                    break;
                    case VariableDeclarationNode varDecl:
                    GenerateVariableDeclaration(varDecl);
                    break;
                    case BlockNode blockStmt:
                    GenerateBlockStatement(blockStmt);
                    break;
                    default:
                    errors.Add($"Line {statement.Line}: Unknown statement type '{statement.GetType().Name}'");
                    break;
            }
        }
        using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace PhoenixCompiler
    {
        public abstract class OptimizationPass
        {
            public abstract List<Instruction> Optimize(List<Instruction> instructions, ProgramNode program);

            protected string GetOptimizationName() => GetType().Name;
        }

        // 1. Constant Folding Optimization
        public class ConstantFoldingPass : OptimizationPass
        {
            public override List<Instruction> Optimize(List<Instruction> instructions, ProgramNode program)
            {
                var optimized = new List<Instruction>();
                var constants = new Dictionary<string, long>();

                for (int i = 0; i < instructions.Count; i++)
                {
                    var inst = instructions[i];
                    var folded = TryFoldConstants(inst, constants);

                    if (folded != null)
                    {
                        optimized.Add(folded);
                        Console.WriteLine($"Constant folding: {inst.Original} -> {folded.Original}");
                    }
                    else
                    {
                        optimized.Add(inst);
                    }

                    UpdateConstants(inst, constants);
                }

                return optimized;
            }

            private Instruction TryFoldConstants(Instruction inst, Dictionary<string, long> constants)
            {
                switch (inst.Operation)
                {
                    case "ADD":
                        return FoldArithmetic(inst, constants, (a, b) => a + b);
                    case "SUB":
                        return FoldArithmetic(inst, constants, (a, b) => a - b);
                    case "IMUL":
                        return FoldArithmetic(inst, constants, (a, b) => a * b);
                    case "MOV":
                        return FoldMove(inst, constants);
                    default:
                        return null;
                }
            }

            private Instruction FoldArithmetic(Instruction inst, Dictionary<string, long> constants, Func<long, long, long> operation)
            {
                if (inst.Operands.Count >= 2)
                {
                    var reg = inst.Operands[0];
                    var operand = inst.Operands[1];

                    if (constants.ContainsKey(reg) && IsConstant(operand, out long value))
                    {
                        var result = operation(constants[reg], value);
                        return new Instruction($"    mov {reg}, {result}");
                    }
                    else if (constants.ContainsKey(reg) && constants.ContainsKey(operand))
                    {
                        var result = operation(constants[reg], constants[operand]);
                        return new Instruction($"    mov {reg}, {result}");
                    }
                }
                return null;
            }

            private Instruction FoldMove(Instruction inst, Dictionary<string, long> constants)
            {
                if (inst.Operands.Count >= 2)
                {
                    var dest = inst.Operands[0];
                    var src = inst.Operands[1];

                    if (constants.ContainsKey(src))
                    {
                        return new Instruction($"    mov {dest}, {constants[src]}");
                    }
                }
                return null;
            }

            private void UpdateConstants(Instruction inst, Dictionary<string, long> constants)
            {
                if (inst.Operation == "MOV" && inst.Operands.Count >= 2)
                {
                    var dest = inst.Operands[0];
                    var src = inst.Operands[1];

                    if (IsConstant(src, out long value))
                    {
                        constants[dest] = value;
                    }
                    else if (constants.ContainsKey(src))
                    {
                        constants[dest] = constants[src];
                    }
                    else
                    {
                        constants.Remove(dest);
                    }
                }
                else if (IsRegisterModifying(inst))
                {
                    foreach (var operand in inst.Operands)
                    {
                        constants.Remove(operand);
                    }
                }
            }

            private bool IsConstant(string operand, out long value)
            {
                return long.TryParse(operand, out value);
            }

            private bool IsRegisterModifying(Instruction inst)
            {
                return new[] { "ADD", "SUB", "IMUL", "IDIV", "INC", "DEC" }.Contains(inst.Operation);
            }
        }

        // 2. Peephole Optimization
        public class PeepholeOptimizationPass : OptimizationPass
        {
            public override List<Instruction> Optimize(List<Instruction> instructions, ProgramNode program)
            {
                var optimized = new List<Instruction>();

                for (int i = 0; i < instructions.Count; i++)
                {
                    var window = GetWindow(instructions, i, 4);
                    var replacement = ApplyPeepholeRules(window);

                    if (replacement != null && replacement.Count < window.Count)
                    {
                        optimized.AddRange(replacement);
                        i += window.Count - 1; // Skip processed instructions
                        Console.WriteLine($"Peephole optimization applied at instruction {i}");
                    }
                    else
                    {
                        optimized.Add(instructions[i]);
                    }
                }

                return optimized;
            }

            private List<Instruction> GetWindow(List<Instruction> instructions, int start, int size)
            {
                var window = new List<Instruction>();
                for (int i = start; i < Math.Min(start + size, instructions.Count); i++)
                {
                    window.Add(instructions[i]);
                }
                return window;
            }

            private List<Instruction> ApplyPeepholeRules(List<Instruction> window)
            {
                // Rule 1: Remove redundant moves (mov rax, rax)
                if (window.Count >= 1 && IsRedundantMove(window[0]))
                {
                    Console.WriteLine($"Removed redundant move: {window[0].Original}");
                    return new List<Instruction>();
                }

                // Rule 2: Optimize mov + mov sequences
                if (window.Count >= 2 && CanOptimizeMovSequence(window[0], window[1]))
                {
                    var optimized = OptimizeMovSequence(window[0], window[1]);
                    Console.WriteLine($"Optimized mov sequence: {window[0].Original} + {window[1].Original}");
                    return optimized;
                }

                // Rule 3: Optimize add/sub with 0
                if (window.Count >= 1 && IsArithmeticWithZero(window[0]))
                {
                    Console.WriteLine($"Removed arithmetic with zero: {window[0].Original}");
                    return new List<Instruction>();
                }

                // Rule 4: Optimize multiply by 1
                if (window.Count >= 1 && IsMultiplyByOne(window[0]))
                {
                    Console.WriteLine($"Removed multiply by one: {window[0].Original}");
                    return new List<Instruction>();
                }

                // Rule 5: Optimize push/pop pairs
                if (window.Count >= 2 && IsPushPopPair(window[0], window[1]))
                {
                    var optimized = OptimizePushPop(window[0], window[1]);
                    Console.WriteLine($"Optimized push/pop pair: {window[0].Original} + {window[1].Original}");
                    return optimized;
                }

                return null; // No optimization applied
            }

            private bool IsRedundantMove(Instruction inst)
            {
                return inst.Operation == "MOV" &&
                       inst.Operands.Count >= 2 &&
                       inst.Operands[0] == inst.Operands[1];
            }

            private bool CanOptimizeMovSequence(Instruction inst1, Instruction inst2)
            {
                return inst1.Operation == "MOV" && inst2.Operation == "MOV" &&
                       inst1.Operands.Count >= 2 && inst2.Operands.Count >= 2 &&
                       inst1.Operands[0] == inst2.Operands[1];
            }

            private List<Instruction> OptimizeMovSequence(Instruction inst1, Instruction inst2)
            {
                // mov A, B; mov B, A -> mov A, B
                if (inst1.Operands[1] == inst2.Operands[0])
                {
                    return new List<Instruction> { inst1 };
                }
                return new List<Instruction> { inst1, inst2 };
            }

            private bool IsArithmeticWithZero(Instruction inst)
            {
                return (inst.Operation == "ADD" || inst.Operation == "SUB") &&
                       inst.Operands.Count >= 2 &&
                       inst.Operands[1] == "0";
            }

            private bool IsMultiplyByOne(Instruction inst)
            {
                return inst.Operation == "IMUL" &&
                       inst.Operands.Count >= 2 &&
                       inst.Operands[1] == "1";
            }

            private bool IsPushPopPair(Instruction inst1, Instruction inst2)
            {
                return inst1.Operation == "PUSH" && inst2.Operation == "POP" &&
                       inst1.Operands.Count >= 1 && inst2.Operands.Count >= 1;
            }

            private List<Instruction> OptimizePushPop(Instruction push, Instruction pop)
            {
                if (push.Operands[0] == pop.Operands[0])
                {
                    // push rax; pop rax -> (remove both)
                    return new List<Instruction>();
                }
                else
                {
                    // push rax; pop rbx -> mov rbx, rax
                    return new List<Instruction>
                {
                    new Instruction($"    mov {pop.Operands[0]}, {push.Operands[0]}")
                };
                }
            }
        }

        // 3. Loop Unrolling Optimization
        public class LoopUnrollingPass : OptimizationPass
        {
            private const int MAX_UNROLL_COUNT = 8;
            private const int MIN_INSTRUCTIONS_TO_UNROLL = 3;

            public override List<Instruction> Optimize(List<Instruction> instructions, ProgramNode program)
            {
                var optimized = new List<Instruction>();
                var loops = IdentifyLoops(instructions);

                int currentIndex = 0;
                foreach (var loop in loops)
                {
                    // Add instructions before the loop
                    while (currentIndex < loop.StartIndex)
                    {
                        optimized.Add(instructions[currentIndex]);
                        currentIndex++;
                    }

                    // Try to unroll the loop
                    var unrolled = TryUnrollLoop(loop, instructions);
                    if (unrolled != null)
                    {
                        optimized.AddRange(unrolled);
                        Console.WriteLine($"Unrolled loop at lines {loop.StartIndex}-{loop.EndIndex}");
                    }
                    else
                    {
                        // Keep original loop
                        for (int i = loop.StartIndex; i <= loop.EndIndex; i++)
                        {
                            optimized.Add(instructions[i]);
                        }
                    }

                    currentIndex = loop.EndIndex + 1;
                }

                // Add remaining instructions
                while (currentIndex < instructions.Count)
                {
                    optimized.Add(instructions[currentIndex]);
                    currentIndex++;
                }

                return optimized;
            }

            private List<LoopInfo> IdentifyLoops(List<Instruction> instructions)
            {
                var loops = new List<LoopInfo>();
                var labels = new Dictionary<string, int>();

                // First pass: identify labels
                for (int i = 0; i < instructions.Count; i++)
                {
                    if (instructions[i].Operation == "LABEL")
                    {
                        labels[instructions[i].Operands[0]] = i;
                    }
                }

                // Second pass: find backward jumps (loops)
                for (int i = 0; i < instructions.Count; i++)
                {
                    var inst = instructions[i];
                    if (IsBranchInstruction(inst) && inst.Operands.Count > 0)
                    {
                        var target = inst.Operands[0];
                        if (labels.ContainsKey(target) && labels[target] < i)
                        {
                            loops.Add(new LoopInfo
                            {
                                StartIndex = labels[target],
                                EndIndex = i,
                                LoopLabel = target,
                                BackJump = inst
                            });
                        }
                    }
                }

                return loops;
            }

            private List<Instruction> TryUnrollLoop(LoopInfo loop, List<Instruction> instructions)
            {
                var loopBody = ExtractLoopBody(loop, instructions);

                // Simple heuristic: only unroll small loops
                if (loopBody.Count > MIN_INSTRUCTIONS_TO_UNROLL && loopBody.Count <= 10)
                {
                    var unrollCount = Math.Min(MAX_UNROLL_COUNT, 4);
                    return UnrollLoop(loopBody, unrollCount, loop);
                }

                return null;
            }

            private List<Instruction> ExtractLoopBody(LoopInfo loop, List<Instruction> instructions)
            {
                var body = new List<Instruction>();

                for (int i = loop.StartIndex + 1; i < loop.EndIndex; i++)
                {
                    if (!IsBranchInstruction(instructions[i]))
                    {
                        body.Add(instructions[i]);
                    }
                }

                return body;
            }

            private List<Instruction> UnrollLoop(List<Instruction> loopBody, int unrollCount, LoopInfo loop)
            {
                var unrolled = new List<Instruction>();

                // Add loop header
                unrolled.Add(new Instruction($"{loop.LoopLabel}:"));

                // Unroll the loop body
                for (int i = 0; i < unrollCount; i++)
                {
                    foreach (var inst in loopBody)
                    {
                        // Create a copy of the instruction with updated labels if necessary
                        var unrolledInst = new Instruction(inst.Original);
                        unrolled.Add(unrolledInst);
                    }
                }

                return unrolled;
            }

            private bool IsBranchInstruction(Instruction inst)
            {
                return new[] { "JMP", "JE", "JNE", "JL", "JG", "JLE", "JGE", "JZ", "JNZ" }
                    .Contains(inst.Operation);
            }

            private class LoopInfo
            {
                public int StartIndex { get; set; }
                public int EndIndex { get; set; }
                public string LoopLabel { get; set; }
                public Instruction BackJump { get; set; }
            }
        }

        // 4. Tail Call Optimization
        public class TailCallOptimizationPass : OptimizationPass
        {
            public override List<Instruction> Optimize(List<Instruction> instructions, ProgramNode program)
            {
                var optimized = new List<Instruction>();
                var functions = IdentifyFunctions(instructions, program);

                foreach (var func in functions)
                {
                    var optimizedFunc = OptimizeTailCalls(func, instructions);
                    optimized.AddRange(optimizedFunc);
                }

                return optimized;
            }

            private List<FunctionInfo> IdentifyFunctions(List<Instruction> instructions, ProgramNode program)
            {
                var functions = new List<FunctionInfo>();

                // Extract function information from the AST
                foreach (var declaration in program.Declarations)
                {
                    if (declaration is FunctionDeclarationNode funcDecl)
                    {
                        var funcInfo = new FunctionInfo
                        {
                            Name = funcDecl.Name,
                            IsRecursive = CheckIfRecursive(funcDecl),
                            Instructions = new List<Instruction>()
                        };

                        functions.Add(funcInfo);
                    }
                }

                // Map instructions to functions
                string currentFunction = null;
                foreach (var inst in instructions)
                {
                    if (inst.Operation == "LABEL" && inst.Original.Contains(":"))
                    {
                        var label = inst.Operands[0];
                        var func = functions.FirstOrDefault(f => f.Name == label);
                        if (func != null)
                        {
                            currentFunction = func.Name;
                        }
                    }

                    if (currentFunction != null)
                    {
                        var func = functions.FirstOrDefault(f => f.Name == currentFunction);
                        func?.Instructions.Add(inst);
                    }
                }

                return functions;
            }

            private List<Instruction> OptimizeTailCalls(FunctionInfo func, List<Instruction> allInstructions)
            {
                var optimized = new List<Instruction>();

                for (int i = 0; i < func.Instructions.Count; i++)
                {
                    var inst = func.Instructions[i];

                    if (IsTailCall(inst, func, i))
                    {
                        var tailCallOpt = OptimizeTailCall(inst, func);
                        optimized.AddRange(tailCallOpt);
                        Console.WriteLine($"Tail call optimization applied in function {func.Name}");
                    }
                    else
                    {
                        optimized.Add(inst);
                    }
                }

                return optimized;
            }

            private bool IsTailCall(Instruction inst, FunctionInfo func, int position)
            {
                // Check if this is a call instruction
                if (inst.Operation != "CALL") return false;

                // Check if it's calling the same function (recursive tail call)
                if (inst.Operands.Count == 0 || inst.Operands[0] != func.Name) return false;

                // Check if it's followed by return instructions
                for (int i = position + 1; i < func.Instructions.Count; i++)
                {
                    var nextInst = func.Instructions[i];
                    if (nextInst.Operation == "RET") return true;
                    if (nextInst.Operation != "MOV" || !nextInst.Original.Contains("rsp") || !nextInst.Original.Contains("rbp"))
                    {
                        return false;
                    }
                }

                return false;
            }

            private List<Instruction> OptimizeTailCall(Instruction tailCall, FunctionInfo func)
            {
                var optimized = new List<Instruction>();

                // Instead of call + return, use jump
                optimized.Add(new Instruction($"    jmp {func.Name}"));

                return optimized;
            }

            private bool CheckIfRecursive(FunctionDeclarationNode func)
            {
                // Simple check for recursive calls in function body
                return ContainsRecursiveCall(func.Body, func.Name);
            }

            private bool ContainsRecursiveCall(StatementNode statement, string functionName)
            {
                switch (statement)
                {
                    case BlockStatementNode block:
                        return block.Statements.Any(s => ContainsRecursiveCall(s, functionName));
                    case ExpressionStatementNode expr:
                        return ContainsRecursiveCall(expr.Expression, functionName);
                    default:
                        return false;
                }
            }

            private bool ContainsRecursiveCall(ExpressionNode expression, string functionName)
            {
                if (expression is FunctionCallExpressionNode call)
                {
                    if (call.Function is IdentifierExpressionNode identifier)
                    {
                        return identifier.Name == functionName;
                    }
                }
                return false;
            }

            private class FunctionInfo
            {
                public string Name { get; set; }
                public bool IsRecursive { get; set; }
                public List<Instruction> Instructions { get; set; }
            }
        }

        // 5. Profile Guided Optimization
        public class ProfileGuidedOptimizationPass : OptimizationPass
        {
            private ProfileData profileData;

            public ProfileGuidedOptimizationPass(ProfileData profileData)
            {
                this.profileData = profileData;
            }

            public override List<Instruction> Optimize(List<Instruction> instructions, ProgramNode program)
            {
                var optimized = new List<Instruction>();

                // Hot path optimization
                var hotPaths = IdentifyHotPaths(instructions);

                // Branch prediction optimization
                optimized = OptimizeBranchPrediction(instructions, hotPaths);

                // Function inlining based on call frequency
                optimized = OptimizeFunctionInlining(optimized, program);

                return optimized;
            }

            private List<string> IdentifyHotPaths(List<Instruction> instructions)
            {
                var hotPaths = new List<string>();

                foreach (var kvp in profileData.BranchTakenCounts.OrderByDescending(x => x.Value))
                {
                    if (kvp.Value > 1000) // Threshold for hot path
                    {
                        hotPaths.Add(kvp.Key);
                        Console.WriteLine($"Identified hot path: {kvp.Key} (taken {kvp.Value} times)");
                    }
                }

                return hotPaths;
            }

            private List<Instruction> OptimizeBranchPrediction(List<Instruction> instructions, List<string> hotPaths)
            {
                var optimized = new List<Instruction>();

                foreach (var inst in instructions)
                {
                    if (IsBranchInstruction(inst) && inst.Operands.Count > 0)
                    {
                        var target = inst.Operands[0];

                        // Reorder branches to put likely targets first
                        if (hotPaths.Contains(target))
                        {
                            // Keep the branch as-is for hot paths
                            optimized.Add(inst);
                            Console.WriteLine($"Branch prediction optimization: keeping hot branch to {target}");
                        }
                        else
                        {
                            // For cold paths, we might invert the condition
                            var inverted = TryInvertBranch(inst);
                            if (inverted != null)
                            {
                                optimized.Add(inverted);
                                Console.WriteLine($"Branch prediction optimization: inverted cold branch");
                            }
                            else
                            {
                                optimized.Add(inst);
                            }
                        }
                    }
                    else
                    {
                        optimized.Add(inst);
                    }
                }

                return optimized;
            }

            private List<Instruction> OptimizeFunctionInlining(List<Instruction> instructions, ProgramNode program)
            {
                var optimized = new List<Instruction>();
                var inlineCandidates = IdentifyInlineCandidates(program);

                foreach (var inst in instructions)
                {
                    if (inst.Operation == "CALL" && inst.Operands.Count > 0)
                    {
                        var funcName = inst.Operands[0];

                        if (inlineCandidates.Contains(funcName))
                        {
                            var inlined = InlineFunction(funcName, program);
                            if (inlined != null)
                            {
                                optimized.AddRange(inlined);
                                Console.WriteLine($"Inlined function: {funcName}");
                                continue;
                            }
                        }
                    }

                    optimized.Add(inst);
                }

                return optimized;
            }

            private List<string> IdentifyInlineCandidates(ProgramNode program)
            {
                var candidates = new List<string>();

                foreach (var declaration in program.Declarations)
                {
                    if (declaration is FunctionDeclarationNode func)
                    {
                        // Consider functions for inlining based on call frequency and size
                        if (profileData.FunctionCallCounts.ContainsKey(func.Name))
                        {
                            var callCount = profileData.FunctionCallCounts[func.Name];
                            var isSmall = IsSmallFunction(func);
                            var isFrequent = callCount > 100;

                            if (isSmall && isFrequent)
                            {
                                candidates.Add(func.Name);
                            }
                        }
                    }
                }

                return candidates;
            }

            private bool IsSmallFunction(FunctionDeclarationNode func)
            {
                // Simple heuristic: count statements in function body
                int statementCount = CountStatements(func.Body);
                return statementCount <= 10;
            }

            private int CountStatements(StatementNode statement)
            {
                switch (statement)
                {
                    case BlockStatementNode block:
                        return block.Statements.Sum(CountStatements);
                    default:
                        return 1;
                }
            }

            private List<Instruction> InlineFunction(string funcName, ProgramNode program)
            {
                // Find the function in the AST
                var func = program.Declarations.OfType<FunctionDeclarationNode>()
                                  .FirstOrDefault(f => f.Name == funcName);

                if (func == null) return null;

                // Generate inline code (simplified)
                var inlined = new List<Instruction>();
                inlined.Add(new Instruction($"    ; Inlined function: {funcName}"));

                // Add function body instructions here
                // This would require re-generating code for the function body

                inlined.Add(new Instruction($"    ; End inlined function: {funcName}"));

                return inlined;
            }

            private bool IsBranchInstruction(Instruction inst)
            {
                return new[] { "JMP", "JE", "JNE", "JL", "JG", "JLE", "JGE", "JZ", "JNZ" }
                    .Contains(inst.Operation);
            }

            private Instruction TryInvertBranch(Instruction branch)
            {
                var inversionMap = new Dictionary<string, string>
                {
                    ["JE"] = "JNE",
                    ["JNE"] = "JE",
                    ["JL"] = "JGE",
                    ["JG"] = "JLE",
                    ["JLE"] = "JG",
                    ["JGE"] = "JL",
                    ["JZ"] = "JNZ",
                    ["JNZ"] = "JZ"
                };

                if (inversionMap.ContainsKey(branch.Operation))
                {
                    var inverted = inversionMap[branch.Operation];
                    return new Instruction($"    {inverted} {string.Join(", ", branch.Operands)}");
                }

                return null;
            }
        }
    }
    using System;
using System.Collections.Generic;
using System.Linq;

namespace PhoenixCompiler
    {
        public class SemanticResult
        {
            public bool Success { get; set; }
            public List<string> Errors { get; set; } = new List<string>();
        }

        public class Symbol
        {
            public string Name { get; set; }
            public string Type { get; set; }
            public bool IsFunction { get; set; }
            public bool IsVariable { get; set; }
            public bool IsType { get; set; }
            public int Line { get; set; }
            public int Column { get; set; }
            public List<ParameterNode> Parameters { get; set; } = new List<ParameterNode>();
        }

        public class Scope
        {
            public Dictionary<string, Symbol> Symbols { get; set; } = new Dictionary<string, Symbol>();
            public Scope Parent { get; set; }

            public Symbol Lookup(string name)
            {
                if (Symbols.ContainsKey(name))
                    return Symbols[name];

                return Parent?.Lookup(name);
            }

            public void Define(Symbol symbol)
            {
                if (Symbols.ContainsKey(symbol.Name))
                    throw new Exception($"Symbol '{symbol.Name}' already defined in this scope");

                Symbols[symbol.Name] = symbol;
            }
        }

        public partial class SemanticAnalyzer
        {
            private Scope currentScope;
            private List<string> errors;
            private Dictionary<string, string> builtinTypes;

            public SemanticAnalyzer()
            {
                builtinTypes = new Dictionary<string, string>
            {
                {"int", "int"}, {"float", "float"}, {"bool", "bool"},
                {"char", "char"}, {"string", "string"}, {"null", "null"},
                {"capsule", "capsule"}, {"stack", "stack"}, {"heap", "heap"},
                {"mutex", "mutex"}, {"thread", "thread"}, {"core", "core"}
            };
            }

            // Complete the missing methods from the cut-off code
            private string VisitBinaryExpression(BinaryExpressionNode binary)
            {
                string leftType = VisitExpression(binary.Left);
                string rightType = VisitExpression(binary.Right);

                switch (binary.Operator)
                {
                    case TokenType.Plus:
                    case TokenType.Minus:
                    case TokenType.Multiply:
                    case TokenType.Divide:
                    case TokenType.Modulo:
                        if (leftType == "int" && rightType == "int")
                            return "int";
                        if (leftType == "float" || rightType == "float")
                            return "float";
                        errors.Add($"Line {binary.Line}: Invalid types for binary operation '{binary.Operator}' ({leftType}, {rightType})");
                        return "error";

                    case TokenType.Equal:
                    case TokenType.NotEqual:
                        if (leftType == rightType || leftType == "null" || rightType == "null")
                            return "bool";
                        errors.Add($"Line {binary.Line}: Cannot compare types '{leftType}' and '{rightType}'");
                        return "error";

                    case TokenType.Greater:
                    case TokenType.GreaterEqual:
                    case TokenType.Less:
                    case TokenType.LessEqual:
                        if ((leftType == "int" || leftType == "float") && (rightType == "int" || rightType == "float"))
                            return "bool";
                        errors.Add($"Line {binary.Line}: Invalid comparison between '{leftType}' and '{rightType}'");
                        return "error";

                    case TokenType.And:
                    case TokenType.Or:
                        if (leftType == "bool" && rightType == "bool")
                            return "bool";
                        errors.Add($"Line {binary.Line}: Logical operation '{binary.Operator}' requires boolean operands");
                        return "error";

                    default:
                        errors.Add($"Line {binary.Line}: Unknown binary operator '{binary.Operator}'");
                        return "error";
                }
            }

            private string VisitUnaryExpression(UnaryExpressionNode unary)
            {
                string operandType = VisitExpression(unary.Operand);

                switch (unary.Operator)
                {
                    case TokenType.Not:
                        if (operandType == "bool")
                            return "bool";
                        errors.Add($"Line {unary.Line}: '!' operator requires boolean operand, got '{operandType}'");
                        return "error";

                    case TokenType.Minus:
                        if (operandType == "int" || operandType == "float")
                            return operandType;
                        errors.Add($"Line {unary.Line}: '-' operator requires numeric operand, got '{operandType}'");
                        return "error";

                    default:
                        errors.Add($"Line {unary.Line}: Unknown unary operator '{unary.Operator}'");
                        return "error";
                }
            }

            private string VisitFunctionCall(FunctionCallExpressionNode call)
            {
                if (call.Function is IdentifierExpressionNode funcId)
                {
                    var functionSymbol = currentScope.Lookup(funcId.Name);
                    if (functionSymbol == null || !functionSymbol.IsFunction)
                    {
                        errors.Add($"Line {call.Line}: Undefined function '{funcId.Name}'");
                        return "error";
                    }

                    // Check argument count
                    if (call.Arguments.Count != functionSymbol.Parameters.Count)
                    {
                        errors.Add($"Line {call.Line}: Function '{funcId.Name}' expects {functionSymbol.Parameters.Count} arguments, got {call.Arguments.Count}");
                        return "error";
                    }

                    // Check argument types
                    for (int i = 0; i < call.Arguments.Count; i++)
                    {
                        string argType = VisitExpression(call.Arguments[i]);
                        string paramType = GetTypeName(functionSymbol.Parameters[i].Type);

                        if (!IsAssignableType(paramType, argType))
                        {
                            errors.Add($"Line {call.Line}: Cannot assign argument of type '{argType}' to parameter of type '{paramType}'");
                            return "error";
                        }
                    }

                    return functionSymbol.Type;
                }

                errors.Add($"Line {call.Line}: Invalid function call");
                return "error";
            }

            private string VisitMemberAccess(MemberAccessExpressionNode member)
            {
                string objectType = VisitExpression(member.Object);
                if (objectType == "error")
                {
                    return "error";
                }

                // Simple member access - in a real implementation, you'd need to track type members
                return "unknown"; // Placeholder
            }

            private string VisitAssignment(AssignmentExpressionNode assignment)
            {
                string leftType = VisitExpression(assignment.Left);
                string rightType = VisitExpression(assignment.Right);

                if (leftType == "error" || rightType == "error")
                {
                    return "error";
                }

                if (!IsAssignableType(leftType, rightType))
                {
                    errors.Add($"Line {assignment.Line}: Cannot assign '{rightType}' to '{leftType}'");
                    return "error";
                }

                return leftType;
            }

            private string VisitNewExpression(NewExpressionNode newExpr)
            {
                string typeName = GetTypeName(newExpr.Type);
                if (!IsValidType(typeName))
                {
                    errors.Add($"Line {newExpr.Line}: Unknown type '{typeName}'");
                    return "error";
                }

                return typeName;
            }

            private string GetTypeName(TypeNode type)
            {
                if (type is PrimitiveTypeNode primitiveType)
                {
                    return primitiveType.Name;
                }

                return "unknown";
            }

            private bool IsValidType(string typeName)
            {
                return builtinTypes.ContainsKey(typeName) || currentScope.Lookup(typeName) != null;
            }

            private bool IsAssignableType(string targetType, string sourceType)
            {
                if (targetType == sourceType || sourceType == "null")
                    return true;

                // Allow implicit numeric conversions
                if (targetType == "float" && sourceType == "int")
                    return true;

                return false;
            }

            private string GetLiteralType(TokenType type)
            {
                switch (type)
                {
                    case TokenType.Integer: return "int";
                    case TokenType.Float: return "float";
                    case TokenType.Boolean: return "bool";
                    case TokenType.Character: return "char";
                    case TokenType.String: return "string";
                    case TokenType.Null: return "null";
                    default: return "unknown";
                }
            }
        }
    }
    using System;
    using System.Collections.Generic;
    using System.Linq;
    namespace PhoenixCompiler
    {
        public class SemanticResult
        {
            public bool Success { get; set; }
            public List<string> Errors { get; set; } = new List<string>();
        }
        public class Symbol
        {
            public string Name { get; set; }
            public string Type { get; set; }
            public bool IsFunction { get; set; }
            public bool IsVariable { get; set; }
            public bool IsType { get; set; }
            public int Line { get; set; }
            public int Column { get; set; }
            public List<ParameterNode> Parameters { get; set; } = new List<ParameterNode>();
        }
        public class Scope
        {
            public Dictionary<string, Symbol> Symbols { get; set; } = new Dictionary<string, Symbol>();
            public Scope Parent { get; set; }
            public Symbol Lookup(string name)
            {
                if (Symbols.ContainsKey(name))
                    return Symbols[name];
                return Parent?.Lookup(name);
            }
            public void Define(Symbol symbol)
            {
                if (Symbols.ContainsKey(symbol.Name))
                    throw new Exception($"Symbol '{symbol.Name}' already defined in this scope");
                Symbols[symbol.Name] = symbol;
            }
        }
        public partial class SemanticAnalyzer
        {
            private Scope currentScope;
            private List<string> errors;
            private Dictionary<string, string> builtinTypes;
            public SemanticAnalyzer()
            {
                builtinTypes = new Dictionary<string, string>
                {
                    {"int", "int"}, {"float", "float"}, {"bool", "bool"},
                    {"char", "char"}, {"string", "string"}, {"null", "null"},
                    {"capsule", "capsule"}, {"stack", "stack"}, {"heap", "heap"},
                    {"mutex", "mutex"}, {"thread", "thread"}, {"core", "core"}
                };
            }
            public SemanticResult Analyze(ProgramNode program)
            {
                currentScope = new Scope();
                errors = new List<string>();
                // First pass: collect symbols
                foreach (var declaration in program.Declarations)
                {
                    VisitDeclaration(declaration);
                }
                // Second pass: check types
                foreach (var statement in program.Statements)
                {
                    VisitStatement(statement);
                }
                return new SemanticResult
                {
                    Success = errors.Count == 0,
                    Errors = errors
                };
            }
            private void VisitDeclaration(DeclarationNode declaration)
            {
                switch (declaration)
                {
                    case FunctionDeclarationNode funcDecl:
                        VisitFunctionDeclaration(funcDecl);
                        break;
                    case VariableDeclarationNode varDecl:
                        VisitVariableDeclaration(varDecl);
                        break;
                    case TypeDeclarationNode typeDecl:
                        VisitTypeDeclaration(typeDecl);
                        break;
                    default:
                        errors.Add($"Unknown declaration type: {declaration.GetType().Name}");
                        break;
                }
            }
            private void VisitFunctionDeclaration(FunctionDeclarationNode funcDecl)
            {
                if (currentScope.Lookup(funcDecl.Name) != null)
                {
                    errors.Add($"Function '{funcDecl.Name}' already defined at line {funcDecl.Line}");
                    return;
                }
                var symbol = new Symbol
                {
                    Name = funcDecl.Name,
                    Type = funcDecl.ReturnType.Name,
                    IsFunction = true,
                    Line = funcDecl.Line,
                    Column = funcDecl.Column
                };
                foreach (var param in funcDecl.Parameters)
                {
                    symbol.Parameters.Add(new ParameterNode
                    {
                        Name = param.Name,
                        Type = param.Type
                    });
                }
                currentScope.Define(symbol);
            }
            private void VisitVariableDeclaration(VariableDeclarationNode varDecl)
            {
                if (currentScope.Lookup(varDecl.Name) != null)
                {
                    errors.Add($"Variable '{varDecl.Name}' already defined at line {varDecl.Line}");
                    return;
                }
                var symbol = new Symbol
                {
                    Name = varDecl.Name,
                    Type = varDecl.Type.Name,
                    IsVariable = true,
                    Line = varDecl.Line,
                    Column = varDecl.Column
                };
                currentScope.Define(symbol);
            }
            private void VisitTypeDeclaration(TypeDeclarationNode typeDecl)
            {
                if (currentScope.Lookup(typeDecl.Name) != null)
                {
                    errors.Add($"Type '{typeDecl.Name}' already defined at line {typeDecl.Line}");
                    return;
                }
                var symbol = new Symbol
                {
                    Name = typeDecl.Name,
                    Type = "type",
                    IsType = true,
                    Line = typeDecl.Line,
                    Column = typeDecl.Column
                };
                currentScope.Define(symbol);
            }
            private void VisitStatement(StatementNode statement)
            {
                switch (statement)
                {
                    case ExpressionStatementNode exprStmt:
                        VisitExpression(exprStmt.Expression);
                        break;
                    case IfStatementNode ifStmt:
                        VisitIfStatement(ifStmt);
                        break;
                    case WhileStatementNode whileStmt:
                        VisitWhileStatement(whileStmt);
                        break;
                    case ForStatementNode forStmt:
                        VisitForStatement(forStmt);
                        break;
                    case ReturnStatementNode returnStmt:
                        VisitReturnStatement(returnStmt);
                        break;
                    default:
                        errors.Add($"Unknown statement type: {statement.GetType().Name}");
                        break;
                }
            }
            private void VisitIfStatement(IfStatementNode ifStmt)
            {
                string conditionType = VisitExpression(ifStmt.Condition);
                if (conditionType != "bool")
                {
                    errors.Add($"Line {ifStmt.Line}: If condition must be boolean, got '{conditionType}'");
                }
                foreach (var stmt in ifStmt.Body)
                {
                    VisitStatement(stmt);
                }
            }
            private void VisitWhileStatement(WhileStatementNode whileStmt)
            {
                string conditionType = VisitExpression(whileStmt.Condition);
                if (conditionType != "bool")
                {
                    errors.Add($"Line {whileStmt.Line}: While condition must be boolean, got '{conditionType}'");
                }
                foreach (var stmt in whileStmt.Body)
                {
                    VisitStatement(stmt);
                }
            }
            private void VisitForStatement(ForStatementNode forStmt)
            {
                string initType = VisitExpression(forStmt.Initializer);
                if (initType != "void")
                {
                    errors.Add($"Line {forStmt.Line}: For loop initializer must be void, got '{initType}'");
                }
                string conditionType = VisitExpression(forStmt.Condition);
                if (conditionType != "bool")
                {
                    errors.Add($"Line {forStmt.Line}: For loop condition must be boolean, got '{conditionType}'");
                }
                string incrementType = VisitExpression(forStmt.Increment);
                if (incrementType != "void")
                {
                    errors.Add($"Line {forStmt.Line}: For loop increment must be void, got '{incrementType}'");
                }
                foreach (var stmt in forStmt.Body)
                {
                    VisitStatement(stmt);
                }
            }
            private void VisitReturnStatement(ReturnStatementNode returnStmt)
            {
                if (returnStmt.Expression != null)
                {
                    string returnType = VisitExpression(returnStmt.Expression);
                    var currentFunction = currentScope.Lookup("currentFunction");
                    if (currentFunction == null || !IsAssignableType(currentFunction.Type, returnType))
                    {
                        errors.Add($"Line {returnStmt.Line}: Cannot return '{returnType}' from function returning '{currentFunction?.Type ?? "void"}'");
                    }
                }
            }
            private string VisitExpression(ExpressionNode expression)
            {
                switch (expression)
                {
                    case BinaryExpressionNode binary:
                        return VisitBinaryExpression(binary);
                    case UnaryExpressionNode unary:
                        return VisitUnaryExpression(unary);
                    case FunctionCallExpressionNode call:
                        return VisitFunctionCall(call);
                    case MemberAccessExpressionNode member:
                        return VisitMemberAccess(member);
                    case AssignmentExpressionNode assignment:
                        return VisitAssignment(assignment);
                    case NewExpressionNode newExpr:
                        return VisitNewExpression(newExpr);
                    case LiteralExpressionNode literal:
                        return GetLiteralType(literal.Type);
                    default:
                        errors.Add($"Unknown expression type: {expression.GetType().Name}");
                        return "error";
                }
            }
            private string VisitBinaryExpression(BinaryExpressionNode binary)
            {
                string leftType = VisitExpression(binary.Left);
                string rightType = VisitExpression(binary.Right);
                switch (binary.Operator)
                {
                    case TokenType.Plus:
                    case TokenType.Minus:
                    case TokenType.Multiply:
                    case TokenType.Divide:
                    case TokenType.Modulo:
                        if (leftType == "int" && rightType == "int")
                            return "int";
                        if (leftType == "float" || rightType == "float")
                            return "float";
                        errors.Add($"Line {binary.Line}: Invalid types for binary operation '{binary.Operator}' ({leftType}, {rightType})");
                        return "error";
                    case TokenType.Equal:
                    case TokenType.NotEqual:
                        if (leftType == rightType || leftType == "null" || rightType == "null")
                            return "bool";
                        errors.Add($"Line {binary.Line}: Cannot compare types '{leftType}' and '{rightType}'");
                        return "error";
                    case TokenType.Greater:
                    case TokenType.GreaterEqual:
                    case TokenType.Less:
                    case TokenType.LessEqual:
                        if ((leftType == "int" || leftType == "float") && (rightType == "int" || rightType == "float"))
                            return "bool";
                        errors.Add($"Line {binary.Line}: Invalid comparison between '{leftType}' and '{rightType}'");
                        return "error";
                    case TokenType.And:
                    case TokenType.Or:
                        if (leftType == "bool" && rightType == "bool")
                            return "bool";
                        errors.Add($"Line {binary.Line}: Logical operation '{binary.Operator}' requires boolean operands");
                        return "error";
                    default:
                        errors.Add($"Line {binary.Line}: Unknown binary operator '{binary.Operator}'");
                        return "error";
                }
            }
            private string VisitUnaryExpression(UnaryExpressionNode unary)
            {
                string operandType = VisitExpression(unary.Operand);
                switch (unary.Operator)
                {
                    case TokenType.Not:
                        if (operandType == "bool")
                            return "bool";
                        errors.Add($"Line {unary.Line}: '!' operator requires boolean operand, got '{operandType}'");
                        return "error";
                    case TokenType.Minus:
                        if (operandType == "int" || operandType == "float")
                            return operandType;
                        errors.Add($"Line {unary.Line}: '-' operator requires numeric operand, got '{operandType}'");
                        return "error";
                    default:
                        errors.Add($"Line {unary.Line}: Unknown unary operator '{unary.Operator}'");
                        return "error";
                }
            }
            private string VisitFunctionCall(FunctionCallExpressionNode call)
            {
                if (call.Function is IdentifierExpressionNode funcId)
                {
                    var functionSymbol = currentScope.Lookup(funcId.Name);
                    if (functionSymbol == null || !functionSymbol.IsFunction)
                    {
                        errors.Add($"Line {call.Line}: Undefined function '{funcId.Name}'");
                        return "error";
                    }
                    // Check argument count
                    if (call.Arguments.Count != functionSymbol.Parameters.Count)
                    {
                        errors.Add($"Line {call.Line}: Function '{funcId.Name}' expects {functionSymbol.Parameters.Count} arguments, got {call.Arguments.Count}");
                        return "error";
                    }
                    // Check argument types
                    for (int i = 0; i < call.Arguments.Count; i++)
                    {
                        string argType = VisitExpression(call.Arguments[i]);
                        string paramType = GetTypeName(functionSymbol.Parameters[i].Type);
                        if (!IsAssignableType(paramType, argType))
                        {
                            errors.Add($"Line {call.Line}: Cannot assign argument of type '{argType}' to parameter of type '{paramType}'");
                            return "error";
                        }
                    }
                    return functionSymbol.Type;
                }
                errors.Add($"Line {call.Line}: Invalid function call");
                return "error";
            }
            private string VisitMemberAccess(MemberAccessExpressionNode member)
            {
                string objectType = VisitExpression(member.Object);
                if (objectType == "error")
                {
                    return "error";
                }
                // Simple member access - in a real implementation, you'd need to track type members
                return "unknown"; // Placeholder
            }
            private string VisitAssignment(AssignmentExpressionNode assignment)
            {
                string leftType = VisitExpression(assignment.Left);
                string rightType = VisitExpression(assignment.Right);
                if (leftType == "error" || rightType == "error")
                {
                    return "error";
                }
                if (!IsAssignableType(leftType, rightType))
                {
                    errors.Add($"Line {assignment.Line}: Cannot assign '{rightType}' to '{leftType}'");
                    return "error";
                }
                return leftType;
            }
            private string VisitNewExpression(NewExpressionNode newExpr)
            {
                string typeName = GetTypeName(newExpr.Type);
                if (!IsValidType(typeName))
                {
                    errors.Add($"Line {newExpr.Line}: Unknown type '{typeName}'");
                    return "error";
                }
                return typeName;
            }
            private string GetTypeName(TypeNode type)
            {
                if (type is PrimitiveTypeNode primitiveType)
                {
                    return primitiveType.Name;
                }
                return "unknown";
            }

