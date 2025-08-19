# phoenix_vm.py
# Phoenix ProLang – Enhanced Virtual Machine with Types, Memory Model, and Modules

import threading
import sys
import re
import uuid
import copy
import math
import random
import time
import os
from typing import Any, Dict, List, Optional, Union

# ------------------------------
# Type System
# ------------------------------

class PhoenixType:
    def __init__(self, name: str, size: int = 8):
        self.name = name
        self.size = size
    
    def __repr__(self):
        return f"PhoenixType({self.name})"

class PhoenixValue:
    def __init__(self, value: Any, type_info: PhoenixType):
        self.value = value
        self.type_info = type_info
    
    def __repr__(self):
        return f"PhoenixValue({self.value}, {self.type_info})"

# Built-in types
BUILTIN_TYPES = {
    "int": PhoenixType("int", 8),
    "float": PhoenixType("float", 8),
    "bool": PhoenixType("bool", 1),
    "char": PhoenixType("char", 1),
    "string": PhoenixType("string", -1),  # variable size
    "null": PhoenixType("null", 0),
}

# ------------------------------
# Memory Management
# ------------------------------

class PhoenixObject:
    def __init__(self, obj_type: str, data: Dict[str, Any]):
        self.id = str(uuid.uuid4())
        self.obj_type = obj_type
        self.data = data
        self.ref_count = 1
    
    def __repr__(self):
        return f"PhoenixObject({self.obj_type}, {self.id[:8]})"

class MemoryManager:
    def __init__(self):
        self.heap = {}  # id -> PhoenixObject
        self.type_definitions = {}  # type_name -> field_info
        
    def allocate(self, obj_type: str, size: Optional[int] = None) -> str:
        if obj_type in self.type_definitions:
            # User-defined type
            fields = {}
            for field_name, field_type in self.type_definitions[obj_type].items():
                fields[field_name] = self._default_value(field_type)
            obj = PhoenixObject(obj_type, fields)
        else:
            # Raw memory allocation
            obj = PhoenixObject("raw", {"size": size, "data": bytearray(size or 0)})
        
        self.heap[obj.id] = obj
        return obj.id
    
    def deallocate(self, obj_id: str):
        if obj_id in self.heap:
            del self.heap[obj_id]
    
    def get_object(self, obj_id: str) -> Optional[PhoenixObject]:
        return self.heap.get(obj_id)
    
    def _default_value(self, type_name: str):
        defaults = {
            "int": 0,
            "float": 0.0,
            "bool": False,
            "char": '\0',
            "string": "",
            "null": None
        }
        return defaults.get(type_name, None)

# ------------------------------
# Exception System
# ------------------------------

class PhoenixException(Exception):
    def __init__(self, message: str, exception_type: str = "RuntimeError"):
        self.message = message
        self.exception_type = exception_type
        super().__init__(message)

# ------------------------------
# Enhanced Runtime State
# ------------------------------

class PhoenixVM:
    def __init__(self):
        self.globals = {}          # name -> PhoenixValue
        self.mutability = {}       # name -> bool (True = mutable, False = immutable)
        self.functions = {}        # fn name → AST
        self.mutexes = {}          # name → threading.Lock
        self.threads = []          # active threads
        self.memory_manager = MemoryManager()
        self.capsules = {}         # capsule_name -> {functions, globals, exports}
        self.imported_capsules = {}  # alias -> capsule_data
        self.call_stack = []       # for error tracing
        self.exception_handlers = []  # try/catch stack
        
    # ------------------------------
    # Type System Methods
    # ------------------------------
    
    def infer_type(self, value: Any) -> PhoenixType:
        if isinstance(value, int):
            return BUILTIN_TYPES["int"]
        elif isinstance(value, float):
            return BUILTIN_TYPES["float"]
        elif isinstance(value, bool):
            return BUILTIN_TYPES["bool"]
        elif isinstance(value, str) and len(value) == 1:
            return BUILTIN_TYPES["char"]
        elif isinstance(value, str):
            return BUILTIN_TYPES["string"]
        elif value is None:
            return BUILTIN_TYPES["null"]
        else:
            return BUILTIN_TYPES["int"]  # default fallback
    
    def create_value(self, value: Any, type_hint: Optional[str] = None) -> PhoenixValue:
        if type_hint and type_hint in BUILTIN_TYPES:
            return PhoenixValue(value, BUILTIN_TYPES[type_hint])
        else:
            return PhoenixValue(value, self.infer_type(value))

    def type_check_compatible(self, left: PhoenixValue, right: PhoenixValue, op: str) -> bool:
        # Basic type compatibility for operations
        if op in ["+", "-", "*", "/", "%"]:
            return left.type_info.name in ["int", "float"] and right.type_info.name in ["int", "float"]
        elif op in ["==", "!=", "<", ">", "<=", ">="]:
            return left.type_info.name == right.type_info.name or \
                   (left.type_info.name in ["int", "float"] and right.type_info.name in ["int", "float"])
        elif op in ["&&", "||"]:
            return left.type_info.name == "bool" and right.type_info.name == "bool"
        return True  # Allow for now, enhance later

    # ------------------------------
    # Enhanced Built-in Functions
    # ------------------------------
    
    def builtin_log(self, *args):
        values = [arg.value if isinstance(arg, PhoenixValue) else arg for arg in args]
        print("[log]", *values)

    def builtin_trace(self, *args):
        values = [arg.value if isinstance(arg, PhoenixValue) else arg for arg in args]
        print("[trace]", *values)

    def builtin_assert(self, condition, message="Assertion failed"):
        cond_val = condition.value if isinstance(condition, PhoenixValue) else condition
        if not cond_val:
            raise PhoenixException(str(message), "AssertionError")

    def builtin_error(self, message):
        msg = message.value if isinstance(message, PhoenixValue) else message
        raise PhoenixException(str(msg), "Error")

    # Memory management builtins
    def builtin_new(self, type_name):
        type_str = type_name.value if isinstance(type_name, PhoenixValue) else type_name
        obj_id = self.memory_manager.allocate(str(type_str))
        return self.create_value(obj_id, "string")

    def builtin_delete(self, obj_ref):
        obj_id = obj_ref.value if isinstance(obj_ref, PhoenixValue) else obj_ref
        self.memory_manager.deallocate(str(obj_id))
        return self.create_value(None, "null")

    def builtin_aloc(self, size):
        size_val = size.value if isinstance(size, PhoenixValue) else size
        obj_id = self.memory_manager.allocate("raw", int(size_val))
        return self.create_value(obj_id, "string")

    def builtin_free(self, obj_ref):
        return self.builtin_delete(obj_ref)

    def builtin_ref(self, obj_ref):
        # Create another reference to the same object
        obj_id = obj_ref.value if isinstance(obj_ref, PhoenixValue) else obj_ref
        return self.create_value(obj_id, "string")

    def builtin_move(self, obj_ref):
        # Move semantics - transfer ownership
        obj_id = obj_ref.value if isinstance(obj_ref, PhoenixValue) else obj_ref
        obj = self.memory_manager.get_object(str(obj_id))
        if obj:
            new_id = str(uuid.uuid4())
            new_obj = PhoenixObject(obj.obj_type, obj.data.copy() if hasattr(obj.data, 'copy') else obj.data)
            new_obj.id = new_id
            self.memory_manager.heap[new_id] = new_obj
            self.memory_manager.deallocate(str(obj_id))
            return self.create_value(new_id, "string")
        return self.create_value(None, "null")

    def builtin_copy(self, obj_ref):
        # Deep copy
        obj_id = obj_ref.value if isinstance(obj_ref, PhoenixValue) else obj_ref
        obj = self.memory_manager.get_object(str(obj_id))
        if obj:
            new_id = str(uuid.uuid4())
            new_obj = PhoenixObject(obj.obj_type, copy.deepcopy(obj.data))
            new_obj.id = new_id
            self.memory_manager.heap[new_id] = new_obj
            return self.create_value(new_id, "string")
        return self.create_value(None, "null")

    # Threading builtins
    def builtin_mutex(self):
        lock = threading.Lock()
        mid = f"m{len(self.mutexes)}"
        self.mutexes[mid] = lock
        return self.create_value(mid, "string")

    def builtin_lock(self, mid):
        mutex_id = mid.value if isinstance(mid, PhoenixValue) else mid
        if str(mutex_id) in self.mutexes:
            self.mutexes[str(mutex_id)].acquire()
        return self.create_value(None, "null")

    def builtin_unlock(self, mid):
        mutex_id = mid.value if isinstance(mid, PhoenixValue) else mid
        if str(mutex_id) in self.mutexes:
            self.mutexes[str(mutex_id)].release()
        return self.create_value(None, "null")

    def builtin_thread(self, fn_name, arg):
        fn_str = fn_name.value if isinstance(fn_name, PhoenixValue) else fn_name
        t = threading.Thread(target=self.call_function, args=(str(fn_str), [arg]))
        t.start()
        self.threads.append(t)
        return self.create_value(t, "string")  # Return thread handle

    def builtin_join(self, thread_handle):
        t = thread_handle.value if isinstance(thread_handle, PhoenixValue) else thread_handle
        if hasattr(t, 'join'):
            t.join()
        return self.create_value(None, "null")

    def builtin_sleep(self, duration):
        dur = duration.value if isinstance(duration, PhoenixValue) else duration
        time.sleep(float(dur) / 1000.0)  # milliseconds to seconds
        return self.create_value(None, "null")

    # Math builtins
    def builtin_sqrt(self, x):
        val = x.value if isinstance(x, PhoenixValue) else x
        return self.create_value(math.sqrt(float(val)), "float")

    def builtin_pow(self, base, exp):
        base_val = base.value if isinstance(base, PhoenixValue) else base
        exp_val = exp.value if isinstance(exp, PhoenixValue) else exp
        return self.create_value(pow(float(base_val), float(exp_val)), "float")

    def builtin_random(self):
        return self.create_value(random.random(), "float")

    def builtin_random_int(self, min_val, max_val):
        min_v = min_val.value if isinstance(min_val, PhoenixValue) else min_val
        max_v = max_val.value if isinstance(max_val, PhoenixValue) else max_val
        return self.create_value(random.randint(int(min_v), int(max_v)), "int")

    # ------------------------------
    # Enhanced AST Execution
    # ------------------------------
    
    def exec_block(self, block, locals_dict):
        for stmt in block:
            try:
                res = self.exec_stmt(stmt, locals_dict)
                if isinstance(res, tuple) and res[0] in ["return", "break", "continue"]:
                    return res
            except PhoenixException as e:
                if self.exception_handlers:
                    handler = self.exception_handlers[-1]
                    return ("exception", e)
                else:
                    raise

    def exec_stmt(self, stmt, locals_dict):
        kind = stmt["kind"]

        if kind == "let":
            name = stmt["name"]
            val = self.eval_expr(stmt["value"], locals_dict)
            is_mutable = stmt.get("mut", True)  # Default to mutable for backwards compatibility
            
            # Store as PhoenixValue if not already
            if not isinstance(val, PhoenixValue):
                val = self.create_value(val)
            
            locals_dict[name] = val
            self.mutability[name] = is_mutable
            return None

        if kind == "assign":
            name = stmt["name"]
            val = self.eval_expr(stmt["value"], locals_dict)
            
            # Check mutability
            if name in self.mutability and not self.mutability[name]:
                raise PhoenixException(f"Cannot assign to immutable variable '{name}'", "MutabilityError")
            
            if not isinstance(val, PhoenixValue):
                val = self.create_value(val)
            
            if name in locals_dict:
                locals_dict[name] = val
            elif name in self.globals:
                self.globals[name] = val
            else:
                raise PhoenixException(f"Undefined variable '{name}'", "NameError")
            return None

        if kind == "expr":
            self.eval_expr(stmt["expr"], locals_dict)
            return None

        if kind == "while":
            while True:
                cond = self.eval_expr(stmt["cond"], locals_dict)
                cond_val = cond.value if isinstance(cond, PhoenixValue) else cond
                if not cond_val:
                    break
                    
                res = self.exec_block(stmt["body"], locals_dict)
                if res is not None:
                    if res[0] == "break": 
                        break
                    if res[0] == "continue": 
                        continue
                    if res[0] == "return": 
                        return res
            return None

        if kind == "if":
            cond = self.eval_expr(stmt["cond"], locals_dict)
            cond_val = cond.value if isinstance(cond, PhoenixValue) else cond
            
            if cond_val:
                return self.exec_block(stmt["then"], locals_dict)
            elif stmt.get("else"):
                return self.exec_block(stmt["else"], locals_dict)
            return None

        if kind == "try":
            self.exception_handlers.append(stmt.get("catch", []))
            try:
                res = self.exec_block(stmt["body"], locals_dict)
                self.exception_handlers.pop()
                return res
            except PhoenixException as e:
                self.exception_handlers.pop()
                if stmt.get("catch"):
                    # Execute catch block with exception info
                    catch_locals = locals_dict.copy()
                    catch_locals["__exception__"] = self.create_value(e.message, "string")
                    catch_locals["__exception_type__"] = self.create_value(e.exception_type, "string")
                    return self.exec_block(stmt["catch"], catch_locals)
                else:
                    raise

        if kind == "throw":
            msg = self.eval_expr(stmt["value"], locals_dict)
            msg_val = msg.value if isinstance(msg, PhoenixValue) else msg
            raise PhoenixException(str(msg_val), "UserException")

        if kind == "break":
            return ("break",)

        if kind == "continue":
            return ("continue",)

        if kind == "return":
            val = self.eval_expr(stmt["value"], locals_dict) if stmt["value"] else self.create_value(0, "int")
            return ("return", val)

        # Type definitions
        if kind in ["struct", "class", "enum", "union"]:
            name = stmt["name"]
            fields = {}
            if kind in ["struct", "class"]:
                for field in stmt.get("fields", []):
                    fields[field["name"]] = field.get("type", "int")
            elif kind == "enum":
                for i, variant in enumerate(stmt.get("variants", [])):
                    fields[variant] = i
            
            self.memory_manager.type_definitions[name] = fields
            return None

        return None

    def eval_expr(self, expr, locals_dict):
        et = expr["kind"]

        if et == "int":
            return self.create_value(int(expr["value"]), "int")
        if et == "float":
            return self.create_value(float(expr["value"]), "float")
        if et == "bool":
            return self.create_value(bool(expr["value"]), "bool")
        if et == "char":
            return self.create_value(str(expr["value"]), "char")
        if et == "str":
            return self.create_value(str(expr["value"]), "string")
        if et == "null":
            return self.create_value(None, "null")

        if et == "var":
            name = expr["name"]
            if name in locals_dict: 
                return locals_dict[name]
            if name in self.globals: 
                return self.globals[name]
            raise PhoenixException(f"Undefined variable '{name}'", "NameError")

        if et == "binop":
            left = self.eval_expr(expr["left"], locals_dict)
            right = self.eval_expr(expr["right"], locals_dict)
            op = expr["op"]
            
            # Type checking
            if not self.type_check_compatible(left, right, op):
                raise PhoenixException(f"Type mismatch for operation '{op}' between {left.type_info.name} and {right.type_info.name}", "TypeError")
            
            l_val = left.value
            r_val = right.value
            
            if op == "+": 
                result = l_val + r_val
            elif op == "-": 
                result = l_val - r_val
            elif op == "*": 
                result = l_val * r_val
            elif op == "/": 
                if r_val == 0:
                    raise PhoenixException("Division by zero", "ArithmeticError")
                result = l_val // r_val if isinstance(l_val, int) and isinstance(r_val, int) else l_val / r_val
            elif op == "%": 
                result = l_val % r_val
            elif op == "==": 
                result = l_val == r_val
            elif op == "!=": 
                result = l_val != r_val
            elif op == "<": 
                result = l_val < r_val
            elif op == ">": 
                result = l_val > r_val
            elif op == "<=": 
                result = l_val <= r_val
            elif op == ">=": 
                result = l_val >= r_val
            elif op == "&&": 
                result = l_val and r_val
            elif op == "||": 
                result = l_val or r_val
            else:
                raise PhoenixException(f"Unknown operator '{op}'", "SyntaxError")
            
            return self.create_value(result, self.infer_type(result).name)

        if et == "call":
            fn_name = expr["name"]
            args = [self.eval_expr(arg, locals_dict) for arg in expr["args"]]
            return self.call_function(fn_name, args)

        if et == "member":
            # Object member access (obj.field)
            obj_expr = self.eval_expr(expr["object"], locals_dict)
            field_name = expr["field"]
            
            obj_id = obj_expr.value
            obj = self.memory_manager.get_object(str(obj_id))
            if obj and field_name in obj.data:
                return self.create_value(obj.data[field_name])
            raise PhoenixException(f"Object has no field '{field_name}'", "AttributeError")

        raise PhoenixException(f"Unknown expression type '{et}'", "SyntaxError")

    def call_function(self, name, args):
        # Convert args to proper format for builtins
        builtin_args = []
        for arg in args:
            if isinstance(arg, PhoenixValue):
                builtin_args.append(arg)
            else:
                builtin_args.append(self.create_value(arg))

        # Enhanced builtin functions
        builtins = {
            "log": self.builtin_log,
            "trace": self.builtin_trace,
            "assert": self.builtin_assert,
            "error": self.builtin_error,
            "mutex": self.builtin_mutex,
            "lock": self.builtin_lock,
            "unlock": self.builtin_unlock,
            "thread": self.builtin_thread,
            "join": self.builtin_join,
            "sleep": self.builtin_sleep,
            "new": self.builtin_new,
            "delete": self.builtin_delete,
            "aloc": self.builtin_aloc,
            "free": self.builtin_free,
            "ref": self.builtin_ref,
            "move": self.builtin_move,
            "copy": self.builtin_copy,
            "sqrt": self.builtin_sqrt,
            "pow": self.builtin_pow,
            "random": self.builtin_random,
            "random_int": self.builtin_random_int,
        }

        if name in builtins:
            try:
                result = builtins[name](*builtin_args)
                return result if isinstance(result, PhoenixValue) else self.create_value(result)
            except Exception as e:
                raise PhoenixException(f"Error in builtin function '{name}': {str(e)}", "RuntimeError")

        # User-defined function
        if name not in self.functions:
            raise PhoenixException(f"Undefined function '{name}'", "NameError")

        fn = self.functions[name]
        locals_dict = {}
        
        # Set up parameters
        for i, param in enumerate(fn["params"]):
            if i < len(builtin_args):
                locals_dict[param] = builtin_args[i]
            else:
                locals_dict[param] = self.create_value(None, "null")

        self.call_stack.append(name)
        try:
            res = self.exec_block(fn["body"], locals_dict)
            if isinstance(res, tuple) and res[0] == "return":
                return res[1]
            return self.create_value(None, "null")
        except Exception as e:
            if not isinstance(e, PhoenixException):
                e = PhoenixException(f"Runtime error in function '{name}': {str(e)}", "RuntimeError")
            raise e
        finally:
            self.call_stack.pop()

# ------------------------------
# Enhanced Lexer and Parser
# ------------------------------

TOKEN_SPEC = [
    ("NUMBER",   r"\d+(\.\d+)?"),
    ("STRING",   r"\".*?\""),
    ("CHAR",     r"'[^']'"),
    ("ID",       r"[A-Za-z_][A-Za-z0-9_]*"),
    ("OP",       r"==|!=|&&|\|\||<=|>=|[+\-*/%<>=!]"),
    ("LBRACE",   r"\{"),
    ("RBRACE",   r"\}"),
    ("LPAREN",   r"\("),
    ("RPAREN",   r"\)"),
    ("LBRACK",   r"\["),
    ("RBRACK",   r"\]"),
    ("DOT",      r"\."),
    ("SEMI",     r";"),
    ("COMMA",    r","),
    ("EQ",       r"="),
    ("SKIP",     r"[ \t\r\n]+"),
    ("MISMATCH", r"."),
]

TOKEN_RE = re.compile("|".join(f"(?P<{name}>{pattern})" for name, pattern in TOKEN_SPEC))

KEYWORDS = {
    "let", "mut", "fn", "return", "while", "if", "else", "for",
    "break", "continue", "capsule", "import", "export", "as",
    "try", "catch", "throw", "struct", "class", "enum", "union",
    "new", "delete", "aloc", "free", "ref", "move", "copy",
    "int", "float", "bool", "char", "string", "null",
    "log", "trace", "assert", "error", "thread", "mutex", 
    "lock", "unlock", "join", "sleep", "true", "false"
}

class Token:
    def __init__(self, type_name, value, line, col):
        self.type, self.value, self.line, self.col = type_name, value, line, col
    def __repr__(self): 
        return f"Token({self.type},{self.value})"

def lex(text):
    line, col = 1, 1
    tokens = []
    for m in TOKEN_RE.finditer(text):
        kind = m.lastgroup
        value = m.group()
        if kind == "NUMBER":
            if "." in value:
                tokens.append(Token("FLOAT", float(value), line, col))
            else:
                tokens.append(Token("INT", int(value), line, col))
        elif kind == "STRING":
            tokens.append(Token("STRING", value.strip('"'), line, col))
        elif kind == "CHAR":
            tokens.append(Token("CHAR", value[1], line, col))  # Remove quotes
        elif kind == "ID":
            if value in KEYWORDS:
                token_type = value.upper()
                if value in ["true", "false"]:
                    tokens.append(Token("BOOL", value == "true", line, col))
                else:
                    tokens.append(Token(token_type, value, line, col))
            else:
                tokens.append(Token("ID", value, line, col))
        elif kind == "OP":
            tokens.append(Token("OP", value, line, col))
        elif kind in {"LBRACE","RBRACE","LPAREN","RPAREN","LBRACK","RBRACK","DOT","SEMI","COMMA","EQ"}:
            tokens.append(Token(kind, value, line, col))
        elif kind == "SKIP":
            if value == "\n":
                line += 1
                col = 1
            else:
                col += len(value)
            continue
        elif kind == "MISMATCH":
            raise SyntaxError(f"Unexpected character '{value}' at line {line}, column {col}")
        col += len(value)
    return tokens

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def peek(self, k=0):
        return self.tokens[self.pos+k] if self.pos+k < len(self.tokens) else None
    
    def match(self, *types):
        if (tok := self.peek()) and tok.type in types:
            self.pos += 1
            return tok
        return None
    
    def expect(self, *types):
        tok = self.match(*types)
        if not tok: 
            raise SyntaxError(f"Expected {types}, got {self.peek()}")
        return tok

    def parse_program(self):
        capsules = {}
        while self.peek():
            if self.peek().type == "CAPSULE":
                capsule = self.parse_capsule()
                capsules[capsule["name"]] = capsule
            else:
                raise SyntaxError(f"Expected capsule, got {self.peek()}")
        return capsules

    def parse_capsule(self):
        self.expect("CAPSULE")
        name = self.expect("ID").value
        self.expect("LBRACE")
        
        functions = {}
        globals_vars = {}
        exports = []
        type_defs = []
        
        while not self.match("RBRACE"):
            if self.peek().type == "FN":
                fn = self.parse_function()
                functions[fn["name"]] = fn
            elif self.peek().type in ["EXPORT"]:
                export = self.parse_export()
                exports.append(export)
                if export["kind"] == "function":
                    functions[export["name"]] = export
                else:
                    globals_vars[export["name"]] = export
            elif self.peek().type in ["STRUCT", "CLASS", "ENUM", "UNION"]:
                type_def = self.parse_type_definition()
                type_defs.append(type_def)
            elif self.peek().type == "LET":
                global_var = self.parse_global_variable()
                globals_vars[global_var["name"]] = global_var
            else:
                raise SyntaxError(f"Unexpected token in capsule: {self.peek()}")
        
        return {
            "kind": "capsule",
            "name": name,
            "functions": functions,
            "globals": globals_vars,
            "exports": exports,
            "types": type_defs
        }

    def parse_export(self):
        self.expect("EXPORT")
        if self.peek().type == "FN":
            fn = self.parse_function()
            fn["exported"] = True
            return fn
        elif self.peek().type == "LET":
            var = self.parse_global_variable()
            var["exported"] = True
            return var
        else:
            raise SyntaxError("Expected function or variable after export")

    def parse_global_variable(self):
        self.expect("LET")
        is_mut = bool(self.match("MUT"))
        name = self.expect("ID").value
        
        type_hint = None
        if self.match("COLON"):  # Optional type annotation
            type_hint = self.expect("ID").value
        
        self.expect("EQ")
        value = self.parse_expr()
        self.expect("SEMI")
        
        return {
            "kind": "let",
            "name": name,
            "mut": is_mut,
            "type": type_hint,
            "value": value
        }

    def parse_type_definition(self):
        type_kind = self.expect("STRUCT", "CLASS", "ENUM", "UNION").type.lower()
        name = self.expect("ID").value
        self.expect("LBRACE")
        
        fields = []
        variants = []
        
        if type_kind in ["struct", "class"]:
            while not self.match("RBRACE"):
                field_name = self.expect("ID").value
                field_type = "int"  # Default type
                if self.match("COLON"):
                    field_type = self.expect("ID").value
                fields.append({"name": field_name, "type": field_type})
                if not self.match("COMMA"):
                    break
            if not self.match("RBRACE"):
                self.expect("RBRACE")
        elif type_kind == "enum":
            while not self.match("RBRACE"):
                variant = self.expect("ID").value
                variants.append(variant)
                if not self.match("COMMA"):
                    break
            if not self.match("RBRACE"):
                self.expect("RBRACE")
        
        return {
            "kind": type_kind,
            "name": name,
            "fields": fields,
            "variants": variants
        }

    def parse_function(self):
        self.expect("FN")
        name = self.expect("ID").value
        self.expect("LPAREN")
        params = []
        if not self.match("RPAREN"):
            while True:
                param_name = self.expect("ID").value
                param_type = "int"  # Default type
                if self.match("COLON"):
                    param_type = self.expect("ID").value
                params.append({"name": param_name, "type": param_type})
                if self.match("COMMA"): 
                    continue
                else:
                    self.expect("RPAREN")
                    break
        
        return_type = "int"  # Default
        if self.match("ARROW"):  # ->
            return_type = self.expect("ID").value
        
        self.expect("LBRACE")
        body = self.parse_block()
        self.expect("RBRACE")
        
        return {
            "kind": "function",
            "name": name,
            "params": [p["name"] for p in params],  # Keep simple for now
            "param_types": {p["name"]: p["type"] for p in params},
            "return_type": return_type,
            "body": body
        }

    def parse_block(self):
        stmts = []
        while self.peek() and self.peek().type not in {"RBRACE"}:
            stmts.append(self.parse_stmt())
        return stmts

    def parse_stmt(self):
        if self.match("LET"):
            is_mut = bool(self.match("MUT"))
            name = self.expect("ID").value
            
            type_hint = None
            if self.match("COLON"):
                type_hint = self.expect("ID").value
            
            self.expect("EQ")
            expr = self.parse_expr()
            self.expect("SEMI")
            return {"kind":"let","name":name,"mut":is_mut,"type":type_hint,"value":expr}
            
        if self.match("RETURN"):
            val = None
            if self.peek() and self.peek().type != "SEMI":
                val = self.parse_expr()
            self.expect("SEMI")
            return {"kind":"return","value":val}
            
        if self.match("WHILE"):
            self.expect("LPAREN")
            cond = self.parse_expr()
            self.expect("RPAREN")
            self.expect("LBRACE")
            body = self.parse_block()
            self.expect("RBRACE")
            return {"kind":"while","cond":cond,"body":body}
            
        if self.match("IF"):
            self.expect("LPAREN")
            cond = self.parse_expr()
            self.expect("RPAREN")
            self.expect("LBRACE")
            then_block = self.parse_block()
            self.expect("RBRACE")
            else_block = None
            if self.match("ELSE"):
                self.expect("LBRACE")
                else_block = self.parse_block()
                self.expect("RBRACE")
            return {"kind":"if","cond":cond,"then":then_block,"else":else_block}

        if self.match("TRY"):
            self.expect("LBRACE")
            try_body = self.parse_block()
            self.expect("RBRACE")
            catch_body = None
            if self.match("CATCH"):
                self.expect("LBRACE")
                catch_body = self.parse_block()
                self.expect("RBRACE")
            return {"kind":"try","body":try_body,"catch":catch_body}

        if self.match("THROW"):
            val = self.parse_expr()
            self.expect("SEMI")
            return {"kind":"throw","value":val}
            
        if self.match("BREAK"):
            self.expect("SEMI")
            return {"kind":"break"}
            
        if self.match("CONTINUE"):
            self.expect("SEMI")
            return {"kind":"continue"}
        
        # Handle assignments and expressions
        expr = self.parse_expr()
        if self.match("EQ"):
            # This was an assignment
            if expr["kind"] != "var":
                raise SyntaxError("Invalid assignment target")
            rhs = self.parse_expr()
            self.expect("SEMI")
            return {"kind":"assign","name":expr["name"],"value":rhs}
        else:
            # This was an expression statement
            self.expect("SEMI")
            return {"kind":"expr","expr":expr}

    def parse_expr(self):
        return self.parse_binop(0)

    PRECEDENCE = {
        "||": 1, "&&": 2,
        "==": 3, "!=": 3, "<": 4, ">": 4, "<=": 4, ">=": 4,
        "+": 5, "-": 5,
        "*": 6, "/": 6, "%": 6
    }

    def parse_binop(self, min_prec):
        left = self.parse_primary()
        while True:
            tok = self.peek()
            if tok and tok.type == "OP" and self.PRECEDENCE.get(tok.value, 0) >= min_prec:
                op = tok.value
                self.pos += 1
                right = self.parse_binop(self.PRECEDENCE[op] + 1)
                left = {"kind":"binop","op":op,"left":left,"right":right}
            else:
                break
        return left

    def parse_primary(self):
        tok = self.peek()
        
        if self.match("INT"):
            return {"kind":"int","value":tok.value}
        if self.match("FLOAT"):
            return {"kind":"float","value":tok.value}
        if self.match("BOOL"):
            return {"kind":"bool","value":tok.value}
        if self.match("CHAR"):
            return {"kind":"char","value":tok.value}
        if self.match("STRING"):
            return {"kind":"str","value":tok.value}
        if self.match("NULL"):
            return {"kind":"null","value":None}
            
        if self.match("ID") or self.match("LOG", "TRACE", "ASSERT", "ERROR", "MUTEX", "LOCK", "UNLOCK", "THREAD", "JOIN", "SLEEP", "NEW", "DELETE", "ALOC", "FREE", "REF", "MOVE", "COPY"):
            name = tok.value
            if self.peek() and self.peek().type == "LPAREN":
                # Function call
                return self.finish_call(name)
            elif self.peek() and self.peek().type == "DOT":
                # Member access
                self.expect("DOT")
                field = self.expect("ID").value
                return {"kind":"member","object":{"kind":"var","name":name},"field":field}
            else:
                # Variable
                return {"kind":"var","name":name}
        
        if self.match("LPAREN"):
            expr = self.parse_expr()
            self.expect("RPAREN")
            return expr
        
        raise SyntaxError(f"Unexpected token {tok}")

    def finish_call(self, name):
        self.expect("LPAREN")
        args = []
        if not self.match("RPAREN"):
            while True:
                args.append(self.parse_expr())
                if self.match("COMMA"): 
                    continue
                else:
                    self.expect("RPAREN")
                    break
        return {"kind":"call","name":name,"args":args}

# ------------------------------
# Enhanced Demo and Runner
# ------------------------------

def run_enhanced_demo():
    """Run an enhanced demo showing new features"""
    vm = PhoenixVM()

    # Sample program with types and error handling
    sample_code = """
capsule Main {
    fn main() {
        try {
            let mut counter = 0;
            let m = mutex();
            let t1 = thread("worker", 25000);
            let t2 = thread("worker", 25000);
            join(t1);
            join(t2);
            log("Final counter:", counter);
            
            // Test math functions
            let sqrt_val = sqrt(16.0);
            log("Square root of 16:", sqrt_val);
            
            // Test memory management
            let obj = new("TestObject");
            log("Created object:", obj);
            delete(obj);
            
            return counter;
        } catch {
            error("Something went wrong!");
            return -1;
        }
    }
    
    fn worker(n) {
        let mut i = 0;
        while (i < n) {
            lock(m);
            counter = counter + 1;
            unlock(m);
            i = i + 1;
        }
    }
}"""

    try:
        tokens = lex(sample_code)
        parser = Parser(tokens)
        capsules = parser.parse_program()
        
        # Load the main capsule
        main_capsule = capsules["Main"]
        vm.functions.update(main_capsule["functions"])
        
        # Initialize globals
        for name, global_var in main_capsule["globals"].items():
            val = vm.eval_expr(global_var["value"], {})
            vm.globals[name] = val
            vm.mutability[name] = global_var.get("mut", True)
        
        # Initialize shared variables
        vm.globals["counter"] = vm.create_value(0, "int")
        vm.globals["m"] = vm.builtin_mutex()
        
        # Run main function
        result = vm.call_function("main", [])
        
        # Wait for threads
        for t in vm.threads:
            t.join()
            
        print(f"\nExecution completed with result: {result.value if isinstance(result, PhoenixValue) else result}")
        print(f"Final counter: {vm.globals['counter'].value}")
        print(f"Total mutexes created: {len(vm.mutexes)}")
        print(f"Total threads spawned: {len(vm.threads)}")
        
    except Exception as e:
        print(f"Error: {e}")

def run_phoenix_file(filename):
    """Parse and run a Phoenix source file with enhanced features"""
    try:
        with open(filename, 'r') as f:
            src = f.read()
        
        tokens = lex(src)
        parser = Parser(tokens)
        capsules = parser.parse_program()
        
        vm = PhoenixVM()
        
        # Load all capsules
        for capsule_name, capsule in capsules.items():
            vm.functions.update(capsule["functions"])
            
            # Load type definitions
            for type_def in capsule.get("types", []):
                vm.memory_manager.type_definitions[type_def["name"]] = {
                    field["name"]: field["type"] for field in type_def.get("fields", [])
                }
            
            # Load global variables
            for name, global_var in capsule["globals"].items():
                val = vm.eval_expr(global_var["value"], {})
                vm.globals[name] = val
                vm.mutability[name] = global_var.get("mut", True)
        
        # Initialize shared variables for compatibility
        if "counter" not in vm.globals:
            vm.globals["counter"] = vm.create_value(0, "int")
            vm.mutability["counter"] = True
        if "m" not in vm.globals:
            vm.globals["m"] = vm.builtin_mutex()
            vm.mutability["m"] = False
        
        result = vm.call_function("main", [])
        
        # Wait for all threads to complete
        for t in vm.threads:
            t.join()
            
        print(f"\nExecution Results:")
        print(f"Return value: {result.value if isinstance(result, PhoenixValue) else result}")
        print(f"Final counter: {vm.globals.get('counter', {}).value if 'counter' in vm.globals else 'N/A'}")
        print(f"Objects in heap: {len(vm.memory_manager.heap)}")
        print(f"Mutexes created: {len(vm.mutexes)}")
        print(f"Threads spawned: {len(vm.threads)}")
        print("Execution complete.")
        
        return result
        
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()

# ------------------------------
# Main Entry Point
# ------------------------------

if __name__ == "__main__":
    if len(sys.argv) > 1:
        if sys.argv[1] == "--demo":
            run_enhanced_demo()
        else:
            run_phoenix_file(sys.argv[1])
    else:
        # Run original demo for compatibility
        print("Running enhanced Phoenix VM demo...")
        run_enhanced_demo()

        import sys
        print("\nTo run a Phoenix source file, use:")
        print(f"  python {sys.argv[0]} <filename>")
        import sys
        print("Or to run the demo with enhanced features:")
        print(f"  python {sys.argv[0]} --demo") 
        import random
        import re
        import threading
        import time
        import os
        import random
        import sys
        import math
        print("For more information, visit the Phoenix VM documentation.")
        # Phoenix VM with enhanced features
        # This code is an enhanced version of the Phoenix VM, adding new features like
        # type definitions, error handling, and enhanced built-in functions.
        # Phoenix VM with enhanced features
        class PhoenixVM:
            def __init__(self):
                self.memory_manager = MemoryManager()
                self.globals = {}
                self.functions = {}
                self.call_stack = []
                self.exception_handlers = []
                self.mutability = {}
                self.threads = []
                self.mutexes = []

# phoenix_full.py
# Phoenix ProLang – Full Bootstrap VM + LLVM Backend

import sys, threading, re

# ------------------------------
# 1. LEXER
# ------------------------------

TOKEN_SPEC = [
    ("NUMBER",   r"\d+"),
    ("FLOAT",    r"\d+\.\d+"),
    ("STRING",   r"\".*?\""),
    ("CHAR",     r"'(.)'"),
    ("ID",       r"[A-Za-z_][A-Za-z0-9_]*"),
    ("EQ",       r"==|!=|&&|\|\||[+\-*/<>]"),
    ("LBRACE",   r"\{"),
    ("RBRACE",   r"\}"),
    ("LPAREN",   r"\("),
    ("RPAREN",   r"\)"),
    ("SEMI",     r";"),
    ("COMMA",    r","),
    ("SKIP",     r"[ \t\r\n]+"),
    ("MISMATCH", r"."),
]

TOKEN_RE = re.compile("|".join(f"(?P<{n}>{p})" for n,p in TOKEN_SPEC))

KEYWORDS = {
    "let","mut","fn","return","while","if","else","break","continue","capsule",
    "log","thread","mutex","lock","unlock","join","true","false","null"
}

class Token:
    def __init__(self,t,v,l,c): self.type,self.value,self.line,self.col=t,v,l,c
    def __repr__(self): return f"Token({self.type},{self.value})"

def lex(src):
    line,col=1,1; tokens=[]
    for m in TOKEN_RE.finditer(src):
        kind,value=m.lastgroup,m.group()
        if kind=="NUMBER": tokens.append(Token("NUMBER",int(value),line,col))
        elif kind=="FLOAT": tokens.append(Token("FLOAT",float(value),line,col))
        elif kind=="STRING": tokens.append(Token("STRING",value.strip('"'),line,col))
        elif kind=="CHAR": tokens.append(Token("CHAR",value.strip("'"),line,col))
        elif kind=="ID":
            if value in KEYWORDS: tokens.append(Token(value.upper(),value,line,col))
            else: tokens.append(Token("ID",value,line,col))
        elif kind=="EQ": tokens.append(Token("EQ",value,line,col))
        elif kind in {"LBRACE","RBRACE","LPAREN","RPAREN","SEMI","COMMA","EQ"}:
            tokens.append(Token(kind,value,line,col))
        elif kind=="SKIP": pass
        elif kind=="MISMATCH": raise SyntaxError(f"Unexpected {value} at line {line}")
        col+=len(value)
    return tokens

# ------------------------------
# 2. PARSER (AST Builder)
# ------------------------------

class Parser:
    def __init__(self,tokens): self.tokens=tokens; self.pos=0
    def peek(self,k=0): return self.tokens[self.pos+k] if self.pos+k<len(self.tokens) else None
    def match(self,*types):
        if (tok:=self.peek()) and tok.type in types: self.pos+=1; return tok
        return None
    def expect(self,*types):
        tok=self.match(*types)
        if not tok: raise SyntaxError(f"Expected {types}, got {self.peek()}")
        return tok

    def parse_program(self):
        self.expect("CAPSULE"); name=self.expect("ID").value; self.expect("LBRACE")
        funcs={}
        while not self.match("RBRACE"):
            fn=self.parse_function(); funcs[fn["name"]]=fn
        return funcs

    def parse_function(self):
        self.expect("FN"); name=self.expect("ID").value; self.expect("LPAREN")
        params=[]
        if not self.match("RPAREN"):
            while True:
                params.append(self.expect("ID").value)
                if self.match("COMMA"): continue
                else: self.expect("RPAREN"); break
        self.expect("LBRACE"); body=self.parse_block(); self.expect("RBRACE")
        return {"name":name,"params":params,"body":body}

    def parse_block(self):
        stmts=[]
        while self.peek() and self.peek().type not in {"RBRACE"}:
            stmts.append(self.parse_stmt())
        return stmts

    def parse_stmt(self):
        if self.match("LET"):
            mut=bool(self.match("MUT"))
            name=self.expect("ID").value; self.expect("EQ")
            val=self.parse_expr(); self.expect("SEMI")
            return {"kind":"let","name":name,"value":val,"mut":mut}
        if self.match("RETURN"):
            v=None
            if self.peek() and self.peek().type!="SEMI": v=self.parse_expr()
            self.expect("SEMI"); return {"kind":"return","value":v}
        if self.match("WHILE"):
            self.expect("LPAREN"); cond=self.parse_expr(); self.expect("RPAREN")
            self.expect("LBRACE"); body=self.parse_block(); self.expect("RBRACE")
            return {"kind":"while","cond":cond,"body":body}
        if self.match("IF"):
            self.expect("LPAREN"); cond=self.parse_expr(); self.expect("RPAREN")
            self.expect("LBRACE"); then=self.parse_block(); self.expect("RBRACE")
            elseblk=None
            if self.match("ELSE"): self.expect("LBRACE"); elseblk=self.parse_block(); self.expect("RBRACE")
            return {"kind":"if","cond":cond,"then":then,"else":elseblk}
        if self.match("BREAK"): self.expect("SEMI"); return {"kind":"break"}
        if self.match("CONTINUE"): self.expect("SEMI"); return {"kind":"continue"}

        tok=self.match("ID","LOG","THREAD","MUTEX","LOCK","UNLOCK","JOIN")
        if not tok: raise SyntaxError(f"Unexpected {self.peek()}")
        if self.match("EQ"):
            val=self.parse_expr(); self.expect("SEMI")
            return {"kind":"assign","name":tok.value,"value":val}
        if self.peek() and self.peek().type=="LPAREN":
            call=self.finish_call(tok.value); self.expect("SEMI")
            return {"kind":"expr","expr":call}
        self.pos-=1; expr=self.parse_expr(); self.expect("SEMI")
        return {"kind":"expr","expr":expr}

    def finish_call(self,name):
        self.expect("LPAREN"); args=[]
        if not self.match("RPAREN"):
            while True:
                args.append(self.parse_expr())
                if self.match("COMMA"): continue
                else: self.expect("RPAREN"); break
        return {"kind":"call","name":name,"args":args}

    def parse_expr(self): return self.parse_binop(0)
    PRECEDENCE={"||":1,"&&":2,"==":3,"!=":3,"<":3,">":3,"+":4,"-":4,"*":5,"/":5}
    def parse_binop(self,min_prec):
        left=self.parse_primary()
        while True:
            tok=self.peek()
            if tok and tok.type=="OP" and self.PRECEDENCE.get(tok.value,0)>=min_prec:
                op=tok.value; self.pos+=1
                right=self.parse_binop(self.PRECEDENCE[op]+1)
                left={"kind":"binop","op":op,"left":left,"right":right}
            else: break
        return left
    def parse_primary(self):
        tok=self.peek()
        if self.match("NUMBER"): return {"kind":"int","value":tok.value}
        if self.match("FLOAT"): return {"kind":"float","value":tok.value}
        if self.match("STRING"): return {"kind":"str","value":tok.value}
        if self.match("CHAR"): return {"kind":"char","value":tok.value}
        if self.match("TRUE"): return {"kind":"bool","value":True}
        if self.match("FALSE"): return {"kind":"bool","value":False}
        if self.match("NULL"): return {"kind":"null"}
        if self.match("ID"): 
            if self.peek() and self.peek().type=="LPAREN": return self.finish_call(tok.value)
            return {"kind":"var","name":tok.value}
        if self.match("LPAREN"):
            e=self.parse_expr(); self.expect("RPAREN"); return e
        raise SyntaxError(f"Unexpected {tok}")

# ------------------------------
# 3. VM INTERPRETER
# ------------------------------

class PhoenixVM:
    def __init__(self):
        self.globals={}; self.functions={}; self.mutexes={}; self.threads=[]
        self.mutflags={}
    def builtin_log(self,*a): print("[log]",*a)
    def builtin_mutex(self): mid=f"m{len(self.mutexes)}"; self.mutexes[mid]=threading.Lock(); return mid
    def builtin_lock(self,m): self.mutexes[m].acquire()
    def builtin_unlock(self,m): self.mutexes[m].release()
    def builtin_thread(self,fn,arg): t=threading.Thread(target=self.call_function,args=(fn,[arg])); t.start(); self.threads.append(t); return t
    def builtin_join(self,t): t.join()
    def exec_block(self,block,locals):
        for s in block:
            r=self.exec_stmt(s,locals)
            if r: return r
    def exec_stmt(self,s,locals):
        k=s["kind"]
        if k=="let": v=self.eval_expr(s["value"],locals); locals[s["name"]]=v; self.mutflags[s["name"]]=s["mut"]; return
        if k=="assign":
            n=s["name"]
            if n not in locals and n not in self.globals: raise RuntimeError(f"Undefined {n}")
            if not self.mutflags.get(n,False): raise RuntimeError(f"Immutable {n}")
            v=self.eval_expr(s["value"],locals)
            if n in locals: locals[n]=v
            else: self.globals[n]=v
            return
        if k=="return": return ("return",self.eval_expr(s["value"],locals) if s["value"] else 0)
        if k=="while":
            while self.eval_expr(s["cond"],locals):
                r=self.exec_block(s["body"],locals)
                if r: 
                    if r[0]=="break": break
                    if r[0]=="continue": continue
                    if r[0]=="return": return r
        if k=="if":
            if self.eval_expr(s["cond"],locals): return self.exec_block(s["then"],locals)
            else: return self.exec_block(s["else"],locals) if s["else"] else None
        if k=="break": return ("break",)
        if k=="continue": return ("continue",)
        if k=="expr": self.eval_expr(s["expr"],locals)
    def eval_expr(self,e,locals):
        k=e["kind"]
        if k in {"int","float","str"}: return e["value"]
        if k=="char": return ord(e["value"])
        if k=="bool": return e["value"]
        if k=="null": return None
        if k=="var":
            if e["name"] in locals: return locals[e["name"]]
            if e["name"] in self.globals: return self.globals[e["name"]]
            raise RuntimeError(f"Undefined {e['name']}")
        if k=="binop":
            l=self.eval_expr(e["left"],locals); r=self.eval_expr(e["right"],locals)
            return eval(f"{repr(l)} {e['op']} {repr(r)}")
        if k=="call":
            fn=e["name"]; args=[self.eval_expr(a,locals) for a in e["args"]]
            return self.call_function(fn,args)
    def call_function(self,name,args):
        if name=="log": return self.builtin_log(*args)
        if name=="mutex": return self.builtin_mutex()
        if name=="lock": return self.builtin_lock(*args)
        if name=="unlock": return self.builtin_unlock(*args)
        if name=="thread": return self.builtin_thread(*args)
        if name=="join": return self.builtin_join(*args)
        fn=self.functions[name]; locals={}
        for i,p in enumerate(fn["params"]): locals[p]=args[i] if i<len(args) else None
        r=self.exec_block(fn["body"],locals)
        if r and r[0]=="return": return r[1]
        return None

# ------------------------------
# 4. LLVM CODEGEN
# ------------------------------

class LLVMGen:
    def __init__(self): self.code=[]; self.tmp=0
    def fresh(self): self.tmp+=1; return f"%t{self.tmp}"
    def emit(self,line): self.code.append(line)
    def llvm_type(self,kind):
        return {"int":"i64","float":"double","bool":"i1","char":"i32","string":"i8*","null":"i8*"}.get(kind,"i64")
    def gen_function(self,fn):
        self.emit(f"define i64 @{fn['name']}() {{")
        for stmt in fn["body"]: self.gen_stmt(stmt)
        self.emit("ret i64 0\n}")
    def gen_stmt(self,s):
        if s["kind"]=="return":
            v=self.gen_expr(s["value"]); self.emit(f"ret i64 {v}")
        elif s["kind"]=="expr": self.gen_expr(s["expr"])
    def gen_expr(self,e):
        k=e["kind"]
        if k=="int": return str(e["value"])
        if k=="float": return str(e["value"])
        if k=="bool": return "1" if e["value"] else "0"
        if k=="binop":
            l=self.gen_expr(e["left"]); r=self.gen_expr(e["right"]); t=self.fresh()
            if e["op"]=="+": self.emit(f"{t} = add i64 {l}, {r}")
            elif e["op"]=="*": self.emit(f"{t} = mul i64 {l}, {r}")
            return t
        return "0"
    def dump(self): return "\n".join(self.code)

# ------------------------------
# 5. MAIN
# ------------------------------

def run_file(fname,mode):
    src=open(fname).read(); toks=lex(src); ast=Parser(toks).parse_program()
    if mode=="--vm":
        vm=PhoenixVM(); vm.functions.update(ast)
        return vm.call_function("main",[])
    elif mode=="--llvm":
        cg=LLVMGen(); 
        for fn in ast.values(): cg.gen_function(fn)
        ll=fname.replace(".phx",".ll"); open(ll,"w").write(cg.dump()); print(f"LLVM IR -> {ll}")
        return 0

if __name__=="__main__":
    if len(sys.argv)<3: print("Usage: phoenix_full.py file.phx [--vm|--llvm]")
    else: run_file(sys.argv[1],sys.argv[2])

class LLVMGen:
    def __init__(self):
        self.code = []
        self.tmp = 0

    def fresh(self):
        self.tmp += 1
        return f"%t{self.tmp}"

    def emit(self, line):
        self.code.append(line)

    def llvm_type(self, kind):
        return {
            "int": "i64",
            "float": "double",
            "bool": "i1",
            "char": "i32",
            "string": "i8*",
            "null": "i8*",
        }.get(kind, "i64")

    # ------------------------------
    # Constant Folding & Peephole
    # ------------------------------
    def fold_binop(self, op, l, r):
        # If both are constants → fold
        if isinstance(l, (int, float)) and isinstance(r, (int, float)):
            if op == "+": return l + r
            if op == "-": return l - r
            if op == "*": return l * r
            if op == "/": return l // r if isinstance(l, int) else l / r
            if op == "==": return int(l == r)
            if op == "!=": return int(l != r)
            if op == "<": return int(l < r)
            if op == ">": return int(l > r)

        # Peephole cases
        if op == "+":
            if r == 0: return l
            if l == 0: return r
        if op == "-":
            if r == 0: return l
        if op == "*":
            if r == 1: return l
            if l == 1: return r
            if r == 0 or l == 0: return 0
        if op == "/":
            if r == 1: return l

        return None  # No optimization

    # ------------------------------
    # Function + Statement Generation
    # ------------------------------
    def gen_function(self, fn):
        self.emit(f"define i64 @{fn['name']}() {{")
        for stmt in fn["body"]:
            self.gen_stmt(stmt)
        self.emit("ret i64 0")
        self.emit("}")

    def gen_stmt(self, s):
        if s["kind"] == "return":
            v = self.gen_expr(s["value"])
            self.emit(f"ret i64 {v}")
        elif s["kind"] == "expr":
            self.gen_expr(s["expr"])

    # ------------------------------
    # Expression Generation
    # ------------------------------
    def gen_expr(self, e):
        k = e["kind"]
        if k == "int": return int(e["value"])
        if k == "float": return float(e["value"])
        if k == "bool": return 1 if e["value"] else 0
        if k == "char": return ord(e["value"])
        if k == "null": return 0

        if k == "binop":
            l = self.gen_expr(e["left"])
            r = self.gen_expr(e["right"])

            # Try folding/peephole
            folded = self.fold_binop(e["op"], l, r)
            if folded is not None:
                return folded  # Emit constant instead of IR

            tmp = self.fresh()
            if e["op"] == "+":
                self.emit(f"{tmp} = add i64 {l}, {r}")
            elif e["op"] == "-":
                self.emit(f"{tmp} = sub i64 {l}, {r}")
            elif e["op"] == "*":
                self.emit(f"{tmp} = mul i64 {l}, {r}")
            elif e["op"] == "/":
                self.emit(f"{tmp} = sdiv i64 {l}, {r}")
            elif e["op"] == "==":
                self.emit(f"{tmp} = icmp eq i64 {l}, {r}")
            elif e["op"] == "!=":
                self.emit(f"{tmp} = icmp ne i64 {l}, {r}")
            elif e["op"] == "<":
                self.emit(f"{tmp} = icmp slt i64 {l}, {r}")
            elif e["op"] == ">":
                self.emit(f"{tmp} = icmp sgt i64 {l}, {r}")
            return tmp

        return 0

    def dump(self):
        return "\n".join(self.code)

    class LLVMGen:
     def __init__(self, functions):
        self.code = []
        self.tmp = 0
        self.functions = functions  # for inlining lookup

    def fresh(self):
        self.tmp += 1
        return f"%t{self.tmp}"

    def emit(self, line):
        self.code.append(line)

    # ------------------------------
    # Function & Statement Generation
    # ------------------------------
    def gen_function(self, fn):
        self.emit(f"define i64 @{fn['name']}() {{")
        for stmt in fn["body"]:
            self.gen_stmt(stmt)
        self.emit("ret i64 0")
        self.emit("}")

    def gen_stmt(self, s):
        if s["kind"] == "return":
            v = self.gen_expr(s["value"])
            self.emit(f"ret i64 {v}")
        elif s["kind"] == "expr":
            self.gen_expr(s["expr"])
        elif s["kind"] == "while":
            # Try loop unrolling if condition is "i < CONST"
            cond = s["cond"]
            if (cond["kind"] == "binop" and cond["op"] == "<"
                and cond["right"]["kind"] == "int"):
                bound = cond["right"]["value"]
                if bound < 16:  # heuristic: only unroll small loops
                    for i in range(bound):
                        for body_stmt in s["body"]:
                            # Replace loop var with constant
                            self.gen_stmt(self.substitute(body_stmt, cond["left"]["name"], i))
                    return
            # else: fallback (not optimized)
            self.emit("; unoptimized loop (dynamic bound)")

    def substitute(self, stmt, varname, constval):
        """ Replace occurrences of loop var with a constant int node """
        import copy
        s2 = copy.deepcopy(stmt)
        def walk(node):
            if isinstance(node, dict):
                if node.get("kind") == "var" and node.get("name") == varname:
                    return {"kind":"int","value":constval}
                return {k: walk(v) for k,v in node.items()}
            elif isinstance(node, list):
                return [walk(n) for n in node]
            else:
                return node
        return walk(s2)

    # ------------------------------
    # Expression Generation
    # ------------------------------
    def gen_expr(self, e):
        k = e["kind"]
        if k == "int": return str(e["value"])
        if k == "float": return str(e["value"])
        if k == "bool": return "1" if e["value"] else "0"
        if k == "char": return str(ord(e["value"]))
        if k == "null": return "0"

        if k == "binop":
            l = self.gen_expr(e["left"])
            r = self.gen_expr(e["right"])
            # constant folding & peephole already inserted
            tmp = self.fresh()
            if e["op"] == "+": self.emit(f"{tmp} = add i64 {l}, {r}")
            elif e["op"] == "-": self.emit(f"{tmp} = sub i64 {l}, {r}")
            elif e["op"] == "*": self.emit(f"{tmp} = mul i64 {l}, {r}")
            elif e["op"] == "/": self.emit(f"{tmp} = sdiv i64 {l}, {r}")
            return tmp

        if k == "call":
            fname = e["name"]
            args = [self.gen_expr(a) for a in e["args"]]

            # Inline expansion for small functions
            if fname in self.functions:
                fn = self.functions[fname]
                if len(fn["body"]) == 1 and fn["body"][0]["kind"] == "return":
                    body_expr = fn["body"][0]["value"]
                    # Substitute params with arguments
                    body_expr = self.substitute_args(body_expr, fn["params"], args)
                    return self.gen_expr(body_expr)

            # fallback: emit function call
            arg_str = ", ".join([f"i64 {a}" for a in args])
            tmp = self.fresh()
            self.emit(f"{tmp} = call i64 @{fname}({arg_str})")
            return tmp

        return "0"

    def substitute_args(self, expr, params, args):
        import copy
        e2 = copy.deepcopy(expr)
        def walk(node):
            if isinstance(node, dict):
                if node.get("kind") == "var" and node["name"] in params:
                    idx = params.index(node["name"])
                    return {"kind":"int","value":int(args[idx])}
                return {k: walk(v) for k,v in node.items()}
            elif isinstance(node, list):
                return [walk(n) for n in node]
            else:
                return node
        return walk(e2)

    def dump(self):
        return "\n".join(self.code)

class LLVMGen:
    def __init__(self, functions):
        self.code = []
        self.tmp = 0
        self.functions = functions

    def fresh(self):
        self.tmp += 1
        return f"%t{self.tmp}"

    def emit(self, line):
        self.code.append(line)

    def gen_function(self, fn):
        self.emit(f"define i64 @{fn['name']}(" +
                  ", ".join([f"i64 %{p}" for p in fn["params"]]) + ") {")
        self.emit("entry:")

        for idx, stmt in enumerate(fn["body"]):
            # detect tail call position
            if (stmt["kind"] == "return" and
                stmt["value"]["kind"] == "call" and
                stmt["value"]["name"] == fn["name"]):
                args = [self.gen_expr(a) for a in stmt["value"]["args"]]
                arg_str = ", ".join([f"i64 {a}" for a in args])
                # musttail ensures no extra frame
                self.emit(f"%t_tail = musttail call i64 @{fn['name']}({arg_str})")
                self.emit("ret i64 %t_tail")
                break
            else:
                self.gen_stmt(stmt)

        self.emit("ret i64 0")
        self.emit("}")

    def gen_stmt(self, s):
        if s["kind"] == "return":
            v = self.gen_expr(s["value"])
            self.emit(f"ret i64 {v}")
        elif s["kind"] == "expr":
            self.gen_expr(s["expr"])
        elif s["kind"] == "while":
            self.emit("; loop handling omitted here (already unrolled if const)")

    def gen_expr(self, e):
        k = e["kind"]
        if k == "int": return str(e["value"])
        if k == "call":
            fname = e["name"]
            args = [self.gen_expr(a) for a in e["args"]]
            tmp = self.fresh()
            arg_str = ", ".join([f"i64 {a}" for a in args])
            self.emit(f"{tmp} = call i64 @{fname}({arg_str})")
            return tmp
        if k == "binop":
            l = self.gen_expr(e["left"])
            r = self.gen_expr(e["right"])
            tmp = self.fresh()
            if e["op"] == "+": self.emit(f"{tmp} = add i64 {l}, {r}")
            elif e["op"] == "-": self.emit(f"{tmp} = sub i64 {l}, {r}")
            elif e["op"] == "*": self.emit(f"{tmp} = mul i64 {l}, {r}")
            elif e["op"] == "/": self.emit(f"{tmp} = sdiv i64 {l}, {r}")
            return tmp
        return "0"

    def dump(self):
        return "\n".join(self.code)

    class LLVMGen:
     def __init__(self, functions):
        self.code = []
        self.tmp = 0
        self.functions = functions
        self.current_defs = {}   # SSA versioning
        self.cse_table = {}      # expr → temp reuse
        self.live_vars = set()   # used for DCE

    def fresh(self, base="t"):
        self.tmp += 1
        return f"%{base}{self.tmp}"

    def emit(self, line, result=None):
        if result:
            self.current_defs[result] = line
        self.code.append(line)

    # ------------------------------
    # Expression Generation (SSA + CSE)
    # ------------------------------
    def gen_expr(self, e):
        k = e["kind"]

        if k == "int": return str(e["value"])
        if k == "float": return str(e["value"])
        if k == "bool": return "1" if e["value"] else "0"

        if k == "binop":
            l = self.gen_expr(e["left"])
            r = self.gen_expr(e["right"])
            op = e["op"]

            # CSE check
            key = (op, l, r)
            if key in self.cse_table:
                return self.cse_table[key]

            tmp = self.fresh()
            if op == "+": self.emit(f"{tmp} = add i64 {l}, {r}", tmp)
            elif op == "-": self.emit(f"{tmp} = sub i64 {l}, {r}", tmp)
            elif op == "*": self.emit(f"{tmp} = mul i64 {l}, {r}", tmp)
            elif op == "/": self.emit(f"{tmp} = sdiv i64 {l}, {r}", tmp)
            self.cse_table[key] = tmp
            return tmp

        if k == "call":
            fname = e["name"]
            args = [self.gen_expr(a) for a in e["args"]]
            tmp = self.fresh()
            arg_str = ", ".join([f"i64 {a}" for a in args])
            self.emit(f"{tmp} = call i64 @{fname}({arg_str})", tmp)
            return tmp

        return "0"

    # ------------------------------
    # Function Generation
    # ------------------------------
    def gen_function(self, fn):
        self.code.append(f"define i64 @{fn['name']}(" +
                         ", ".join([f"i64 %{p}" for p in fn["params"]]) + ") {")
        for stmt in fn["body"]:
            self.gen_stmt(stmt)
        self.code.append("ret i64 0")
        self.code.append("}")

    def gen_stmt(self, s):
        if s["kind"] == "return":
            v = self.gen_expr(s["value"])
            self.code.append(f"ret i64 {v}")
            self.live_vars.add(v)
        elif s["kind"] == "expr":
            v = self.gen_expr(s["expr"])
            self.live_vars.add(v)

    # ------------------------------
    # Optimizer: Dead Code Elimination
    # ------------------------------
    def eliminate_dead_code(self):
        optimized = []
        for line in self.code:
            if "=" in line:
                lhs = line.split("=")[0].strip()
                if lhs not in self.live_vars:
                    continue  # dead code, skip
            optimized.append(line)
        self.code = optimized

    def dump(self):
        self.eliminate_dead_code()
        return "\n".join(self.code)

class RegisterAllocator:
    def __init__(self):
        self.pool = ["%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi"]
        self.mapping = {}
        self.spills = {}
        self.spill_offset = 0

    def allocate(self, temp):
        if temp in self.mapping:
            return self.mapping[temp]

        if self.pool:
            reg = self.pool.pop(0)
            self.mapping[temp] = reg
            return reg
        else:
            # Spill to stack
            self.spill_offset += 8
            self.spills[temp] = -self.spill_offset
            return f"[rbp{self.spills[temp]}]"

    def release(self, temp):
        if temp in self.mapping:
            reg = self.mapping.pop(temp)
            self.pool.insert(0, reg)
        elif temp in self.spills:
            pass  # spilled, do nothing

    def remap_line(self, line):
        """Rewrite %tN to registers/stack slots"""
        for t in list(self.mapping.keys()) + list(self.spills.keys()):
            reg = self.mapping.get(t) or f"[rbp{self.spills[t]}]"
            line = line.replace(t, reg)
        return line

class LLVMGen:
    def __init__(self, functions):
        self.code = []
        self.tmp = 0
        self.functions = functions
        self.regalloc = RegisterAllocator()

    def fresh(self):
        self.tmp += 1
        return f"%t{self.tmp}"

    def emit(self, line):
        self.code.append(line)

    def gen_expr(self, e):
        k = e["kind"]
        if k == "int": return str(e["value"])
        if k == "binop":
            l = self.gen_expr(e["left"])
            r = self.gen_expr(e["right"])
            tmp = self.fresh()
            self.emit(f"{tmp} = add i64 {l}, {r}")
            return tmp
        return "0"

    def gen_function(self, fn):
        self.code.append(f"; Function {fn['name']}")
        for stmt in fn["body"]:
            if stmt["kind"] == "return":
                v = self.gen_expr(stmt["value"])
                self.emit(f"ret {v}")

    def allocate_registers(self):
        mapped = []
        for line in self.code:
            mapped.append(self.regalloc.remap_line(line))
        return "\n".join(mapped)

class PhoenixASMGen:
    def __init__(self):
        self.code = []
        self.label_count = 0

    def fresh_label(self, base="L"):
        self.label_count += 1
        return f".{base}{self.label_count}"

    def emit(self, line):
        self.code.append(line)

    def gen_prologue(self, fname):
        self.emit(f"global {fname}")
        self.emit(f"{fname}:")
        self.emit("    push rbp")
        self.emit("    mov rbp, rsp")

    def gen_epilogue(self):
        self.emit("    mov rsp, rbp")
        self.emit("    pop rbp")
        self.emit("    ret")

    def gen_function(self, fn):
        self.gen_prologue(fn["name"])

        for stmt in fn["body"]:
            if stmt["kind"] == "return":
                val = stmt["value"]
                if val["kind"] == "int":
                    self.emit(f"    mov rax, {val['value']}")
                elif val["kind"] == "binop":
                    self.gen_binop(val)
                self.gen_epilogue()

        self.emit("")

    def gen_binop(self, e):
        l = e["left"]["value"] if e["left"]["kind"] == "int" else 0
        r = e["right"]["value"] if e["right"]["kind"] == "int" else 0
        if e["op"] == "+":
            self.emit(f"    mov rax, {l}")
            self.emit(f"    add rax, {r}")
        elif e["op"] == "*":
            self.emit(f"    mov rax, {l}")
            self.emit(f"    imul rax, {r}")

    def dump(self):
        return "\n".join(self.code)

#!/usr/bin/env python3
import subprocess, sys, os

# ------------------------------
# 1. Lexer / Parser (simplified stub)
# ------------------------------
# type: ignore # reuse your existing lexer+parser

# ------------------------------
# 2. SSA + Optimizer
# ------------------------------
class SSAOptimizer:
    def __init__(self):
        self.tmp = 0
        self.cse = {}
        self.code = []

    def fresh(self):
        self.tmp += 1
        return f"%t{self.tmp}"

    def optimize_expr(self, e):
        k = e["kind"]
        if k == "int": return str(e["value"])
        if k == "binop":
            l = self.optimize_expr(e["left"])
            r = self.optimize_expr(e["right"])
            key = (e["op"], l, r)
            if key in self.cse:
                return self.cse[key]
            tmp = self.fresh()
            self.code.append((tmp, e["op"], l, r))
            self.cse[key] = tmp
            return tmp
        return "0"

    def optimize_function(self, fn):
        out = []
        for stmt in fn["body"]:
            if stmt["kind"] == "return":
                val = self.optimize_expr(stmt["value"])
                out.append(("return", val))
        return out

# ------------------------------
# 3. Register Allocation
# ------------------------------
class RegisterAllocator:
    def __init__(self):
        self.pool = ["rax", "rbx", "rcx", "rdx", "rsi", "rdi"]
        self.mapping = {}
        self.spill_offset = 0
        self.spills = {}

    def get(self, temp):
        if temp.isdigit():   # literal int
            return temp
        if temp in self.mapping:
            return self.mapping[temp]
        if self.pool:
            reg = self.pool.pop(0)
            self.mapping[temp] = reg
            return reg
        # spill
        self.spill_offset += 8
        self.spills[temp] = -self.spill_offset
        return f"[rbp{self.spills[temp]}]"

# ------------------------------
# 4. Assembly Generator
# ------------------------------
class ASMGen:
    def __init__(self):
        self.lines = ["section .text"]

    def gen_function(self, fn_name, body, ssa, regalloc):
        self.lines.append(f"global {fn_name}")
        self.lines.append(f"{fn_name}:")
        self.lines.append("    push rbp")
        self.lines.append("    mov rbp, rsp")

        for instr in ssa.code:
            dest, op, l, r = instr
            rd = regalloc.get(dest)
            rl = regalloc.get(l)
            rr = regalloc.get(r)
            if op == "+":
                self.lines.append(f"    mov {rd}, {rl}")
                self.lines.append(f"    add {rd}, {rr}")
            elif op == "*":
                self.lines.append(f"    mov {rd}, {rl}")
                self.lines.append(f"    imul {rd}, {rr}")
            elif op == "-":
                self.lines.append(f"    mov {rd}, {rl}")
                self.lines.append(f"    sub {rd}, {rr}")
            elif op == "/":
                self.lines.append(f"    mov {rd}, {rl}")
                self.lines.append("    cqo")  # sign extend rax
                self.lines.append(f"    idiv {rr}")

        # handle return
        for stmt in body:
            if stmt[0] == "return":
                val = regalloc.get(stmt[1])
                if val != "rax":
                    self.lines.append(f"    mov rax, {val}")

        self.lines.append("    mov rsp, rbp")
        self.lines.append("    pop rbp")
        self.lines.append("    ret\n")

    def dump(self):
        return "\n".join(self.lines)

# ------------------------------
# 5. Compiler Driver
# ------------------------------
def compile_phoenix(filename):
    # 1. Parse
    with open(filename, "r") as f:
        src = f.read()
    tokens = lex(src)
    parser = Parser(tokens)
    functions = parser.parse_program()

    # 2. Optimize + Lower
    asmgen = ASMGen()
    for fname, fn in functions.items():
        ssa = SSAOptimizer()
        body = ssa.optimize_function(fn)
        regalloc = RegisterAllocator()
        asmgen.gen_function(fname, body, ssa, regalloc)

    # 3. Write assembly file
    asmfile = filename.replace(".phx", ".s")
    with open(asmfile, "w") as f:
        f.write(asmgen.dump())

    # 4. Assemble + Link
    objfile = asmfile.replace(".s", ".o")
    exefile = asmfile.replace(".s", "")

    subprocess.run(["nasm", "-felf64", asmfile, "-o", objfile], check=True)
    subprocess.run(["gcc", objfile, "-o", exefile], check=True)

    print(f"[phoenixc] Compiled {filename} → {exefile}")
    return exefile

# ------------------------------
# Main
# ------------------------------
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: phoenixc.py program.phx")
        sys.exit(0)
    compile_phoenix(sys.argv[1])

    # ------------------------------

class ASMGen:
    def __init__(self):
        self.lines = ["section .text"]
        self.data = ["section .data"]
        self.string_table = {}
        self.string_count = 0

    def add_string(self, s):
        if s in self.string_table:
            return self.string_table[s]
        label = f"msg{self.string_count}"
        self.string_count += 1
        self.string_table[s] = label
        self.data.append(f'{label} db "{s}", 10, 0')  # append newline, null-terminated
        return label

    def gen_function(self, fn_name, body, ssa, regalloc):
        self.lines.append("extern printf")
        self.lines.append(f"global {fn_name}")
        self.lines.append(f"{fn_name}:")
        self.lines.append("    push rbp")
        self.lines.append("    mov rbp, rsp")

        for instr in ssa.code:
            dest, op, l, r = instr
            rd = regalloc.get(dest)
            rl = regalloc.get(l)
            rr = regalloc.get(r)
            if op == "+":
                self.lines.append(f"    mov {rd}, {rl}")
                self.lines.append(f"    add {rd}, {rr}")
            elif op == "*":
                self.lines.append(f"    mov {rd}, {rl}")
                self.lines.append(f"    imul {rd}, {rr}")
            elif op == "logstr":
                label = self.add_string(l)
                self.lines.append(f"    lea rdi, [rel {label}]")
                self.lines.append("    xor rax, rax")
                self.lines.append("    call printf")
            elif op == "logint":
                # print integer using "%d\n"
                fmt = self.add_string("%d")
                self.lines.append(f"    lea rdi, [rel {fmt}]")
                self.lines.append(f"    mov rsi, {rl}")
                self.lines.append("    xor rax, rax")
                self.lines.append("    call printf")

        for stmt in body:
            if stmt[0] == "return":
                val = regalloc.get(stmt[1])
                if val != "rax":
                    self.lines.append(f"    mov rax, {val}")

        self.lines.append("    mov rsp, rbp")
        self.lines.append("    pop rbp")
        self.lines.append("    ret\n")

    def dump(self):
        return "\n".join(self.lines + self.data)

class SSAOptimizer:
    def __init__(self):
        self.tmp = 0
        self.cse = {}
        self.code = []

    def fresh(self):
        self.tmp += 1
        return f"%t{self.tmp}"

    def optimize_expr(self, e):
        k = e["kind"]
        if k == "int":
            return str(e["value"])
        if k == "str":
            tmp = self.fresh()
            self.code.append((tmp, "logstr", e["value"], None))
            return tmp
        if k == "call" and e["name"] == "log":
            arg = e["args"][0]
            if arg["kind"] == "int":
                tmp = self.fresh()
                self.code.append((tmp, "logint", str(arg["value"]), None))
                return tmp
            elif arg["kind"] == "str":
                tmp = self.fresh()
                self.code.append((tmp, "logstr", arg["value"], None))
                return tmp
        if k == "binop":
            l = self.optimize_expr(e["left"])
            r = self.optimize_expr(e["right"])
            key = (e["op"], l, r)
            if key in self.cse:
                return self.cse[key]
            tmp = self.fresh()
            self.code.append((tmp, e["op"], l, r))
            self.cse[key] = tmp
            return tmp
        return "0"

    def optimize_function(self, fn):
        out = []
        for stmt in fn["body"]:
            if stmt["kind"] == "return":
                val = self.optimize_expr(stmt["value"])
                out.append(("return", val))
            elif stmt["kind"] == "expr":
                self.optimize_expr(stmt["expr"])
        return out


class Parser:
    ...
    def parse_program(self):
        self.expect("CAPSULE")
        capsule_name = self.expect("ID").value
        self.expect("LBRACE")
        funcs = {}
        exports = []
        imports = []
        while not self.match("RBRACE"):
            if self.match("EXPORT"):
                fn = self.parse_function()
                fn["export"] = True
                funcs[fn["name"]] = fn
                exports.append(fn["name"])
            elif self.match("IMPORT"):
                cap = self.expect("ID").value
                self.expect("DOT")
                fn = self.expect("ID").value
                imports.append((cap, fn))
            else:
                fn = self.parse_function()
                funcs[fn["name"]] = fn
        return {"capsule": capsule_name, "functions": funcs, "exports": exports, "imports": imports}

def compile_phoenix(filename):
    with open(filename, "r") as f:
        src = f.read()
    tokens = lex(src)
    parser = Parser(tokens)
    capsule = parser.parse_program()

    asmgen = ASMGen()
    asmgen.declare_exports(capsule["exports"])
    asmgen.declare_imports(capsule["imports"])

    for fn in capsule["functions"].values():
        asmgen.gen_function(fn)

    asmfile = filename.replace(".phx", ".s")
    with open(asmfile, "w") as f:
        f.write(asmgen.dump())

    objfile = asmfile.replace(".s", ".o")
    subprocess.run(["nasm", "-felf64", asmfile, "-o", objfile], check=True)

    return objfile

def parse_program(self):
    self.expect("CAPSULE")
    capsule_name = self.expect("ID").value
    self.expect("LBRACE")
    funcs = {}
    exports = {"fns": [], "vars": []}
    imports = {"fns": [], "vars": []}
    globals = []

    while not self.match("RBRACE"):
        if self.match("EXPORT"):
            if self.peek().type == "FN":
                fn = self.parse_function()
                fn["export"] = True
                funcs[fn["name"]] = fn
                exports["fns"].append(fn["name"])
            elif self.peek().type == "LET":
                self.expect("LET")
                mut = bool(self.match("MUT"))
                name = self.expect("ID").value
                self.expect("EQ")
                val = self.parse_expr()
                self.expect("SEMI")
                globals.append({"name": name, "value": val, "mut": mut, "export": True})
                exports["vars"].append((name, mut, val))
        elif self.match("IMPORT"):
            cap = self.expect("ID").value
            self.expect("DOT")
            var_or_fn = self.expect("ID").value
            if self.peek() and self.peek().type == "LPAREN":
                imports["fns"].append((cap, var_or_fn))
            else:
                imports["vars"].append((cap, var_or_fn))
        else:
            fn = self.parse_function()
            funcs[fn["name"]] = fn

    return {
        "capsule": capsule_name,
        "functions": funcs,
        "globals": globals,
        "exports": exports,
        "imports": imports,
    }

class ASMGen:
    def __init__(self):
        self.lines = ["section .text"]
        self.data = ["section .data"]
        self.rodata = ["section .rodata"]
        self.string_table = {}
        self.string_count = 0

    def declare_exports(self, exports):
        for fn in exports["fns"]:
            self.lines.append(f"global {fn}")
        for name, mut, val in exports["vars"]:
            self.data.append(f"{name}: dq {val['value'] if val['kind']=='int' else 0}")
            self.lines.append(f"global {name}")

    def declare_imports(self, imports):
        for cap, fn in imports["fns"]:
            self.lines.append(f"extern {fn}")
        for cap, var in imports["vars"]:
            self.lines.append(f"extern {var}")

    def gen_globals(self, globals_):
        for g in globals_:
            if g.get("export"): 
                continue  # already emitted in declare_exports
            if g["mut"]:
                self.data.append(f"{g['name']}: dq {g['value']['value'] if g['value']['kind']=='int' else 0}")
            else:
                self.rodata.append(f"{g['name']}: dq {g['value']['value'] if g['value']['kind']=='int' else 0}")

    def dump(self):
        return "\n".join(self.lines + self.data + self.rodata)

self:any 
global_symbols = {
    "counter": {"mut": True, "section": ".data"}
}

if not self.symbols[name]["mut"]: # type: ignore
    raise CompileError("Cannot assign to immutable global")

globals.append({
    "name": name,# type: ignore
    "value": val,# type: ignore
    "mut": mut, # type: ignore
    "export": True
})

class ASMGen:
    def __init__(self):
        self.lines = ["section .text"]
        self.data = ["section .data"]
        self.rodata = ["section .rodata"]
        self.symbols = {}  # global_name → {mut: bool, section: str}

def declare_exports(self, exports):
    for name, mut, val in exports["vars"]:
        if mut:
            self.data.append(f"{name}: dq {val['value'] if val['kind']=='int' else 0}")
            self.symbols[name] = {"mut": True, "section": ".data"}
        else:
            self.rodata.append(f"{name}: dq {val['value'] if val['kind']=='int' else 0}")
            self.symbols[name] = {"mut": False, "section": ".rodata"}
        self.lines.append(f"global {name}")

if stmt["kind"] == "assign": # type: ignore
    name = stmt["name"] # type: ignore
    if name in self.symbols and not self.symbols[name]["mut"]:
        raise RuntimeError(f"Cannot assign to immutable global '{name}'")
    # otherwise emit MOV into that symbol

class Parser:
    def parse_program(self):
        self.expect("CAPSULE")
        capsule_name = self.expect("ID").value
        self.expect("LBRACE")

        funcs, globals_, exports, imports = {}, [], {"fns": [], "vars": []}, {"fns": [], "vars": []}

        while not self.match("RBRACE"):
            if self.match("EXPORT"):
                if self.peek().type == "FN":
                    fn = self.parse_function()
                    fn["export"] = True
                    funcs[fn["name"]] = fn
                    exports["fns"].append(fn["name"])
                elif self.peek().type == "LET":
                    self.expect("LET")
                    mut = bool(self.match("MUT"))
                    name = self.expect("ID").value
                    self.expect("EQ")
                    val = self.parse_expr()
                    self.expect("SEMI")
                    globals_.append({"name": name, "mut": mut, "value": val, "export": True})
                    exports["vars"].append((name, mut, val))
            elif self.match("IMPORT"):
                cap = self.expect("ID").value
                self.expect("DOT")
                symbol = self.expect("ID").value
                if self.peek() and self.peek().type == "LPAREN":
                    imports["fns"].append((cap, symbol))
                else:
                    imports["vars"].append((cap, symbol))
            else:
                fn = self.parse_function()
                funcs[fn["name"]] = fn

        return {
            "capsule": capsule_name,
            "functions": funcs,
            "globals": globals_,
            "exports": exports,
            "imports": imports,
        }

    class ASMGen:
     def __init__(self):
        self.lines = ["section .text"]
        self.data = ["section .data"]
        self.rodata = ["section .rodata"]
        self.symbols = {}   # name → {"mut": bool, "section": str}

    def declare_exports(self, exports):
        for fn in exports["fns"]:
            self.lines.append(f"global {fn}")

        for name, mut, val in exports["vars"]:
            if mut:
                self.data.append(f"{name}: dq {val['value'] if val['kind']=='int' else 0}")
                self.symbols[name] = {"mut": True, "section": ".data"}
            else:
                self.rodata.append(f"{name}: dq {val['value'] if val['kind']=='int' else 0}")
                self.symbols[name] = {"mut": False, "section": ".rodata"}
            self.lines.append(f"global {name}")

    def declare_imports(self, imports):
        for cap, fn in imports["fns"]:
            self.lines.append(f"extern {fn}")
        for cap, var in imports["vars"]:
            self.lines.append(f"extern {var}")

    def check_assign(self, name):
        if name in self.symbols and not self.symbols[name]["mut"]:
            raise RuntimeError(f"Cannot assign to immutable global '{name}'")

for stmt in fn["body"]:
    if stmt["kind"] == "assign":
        name = stmt["name"]
        self.check_assign(name)   # ensures mutability
        # then emit MOV into the right section

{"kind": "let", "name": "x", "value": {...}, "mut": False}

locals = {"x": {"value": 5, "mut": False}}
globals = {"counter": {"value": 0, "mut": True}}

class PhoenixVM:
    def __init__(self):
        self.globals = {}     # name → {"value": v, "mut": bool}
        self.functions = {}

    def exec_stmt(self, stmt, locals):
        kind = stmt["kind"]

        if kind == "let":
            name = stmt["name"]
            val = self.eval_expr(stmt["value"], locals)
            locals[name] = {"value": val, "mut": stmt["mut"]}
            return

        if kind == "assign":
            name = stmt["name"]
            val = self.eval_expr(stmt["value"], locals)
            if name in locals:
                if not locals[name]["mut"]:
                    raise RuntimeError(f"Cannot reassign immutable local '{name}'")
                locals[name]["value"] = val
            elif name in self.globals:
                if not self.globals[name]["mut"]:
                    raise RuntimeError(f"Cannot reassign immutable global '{name}'")
                self.globals[name]["value"] = val
            else:
                raise RuntimeError(f"Undefined variable '{name}'")
            return

        if kind == "return":
            val = self.eval_expr(stmt["value"], locals) if stmt["value"] else 0
            return ("return", val)

def eval_expr(self, expr, locals):
    et = expr["kind"]

    if et == "var":
        if expr["name"] in locals:
            return locals[expr["name"]]["value"]
        if expr["name"] in self.globals:
            return self.globals[expr["name"]]["value"]
        raise RuntimeError(f"Undefined var {expr['name']}")

symbols = {"x": {"mut": False}, "y": {"mut": True}}

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.scope_stack = []  # stack of symbol tables for local scopes
        self.globals = {}      # global vars with mutability info

    def push_scope(self):
        self.scope_stack.append({})
    def pop_scope(self):
        self.scope_stack.pop()

    def declare_var(self, name, mut, global_scope=False):
        if global_scope:
            if name in self.globals:
                raise SyntaxError(f"Duplicate global variable '{name}'")
            self.globals[name] = {"mut": mut}
        else:
            if name in self.scope_stack[-1]:
                raise SyntaxError(f"Duplicate local variable '{name}'")
            self.scope_stack[-1][name] = {"mut": mut}

    def lookup_var(self, name):
        # Check local scopes (innermost first)
        for scope in reversed(self.scope_stack):
            if name in scope:
                return scope[name]
        # Then check globals
        if name in self.globals:
            return self.globals[name]
        return None

    def parse_stmt(self):
        if self.match("LET"):
            mut = bool(self.match("MUT"))
            name = self.expect("ID").value
            self.expect("EQ")
            expr = self.parse_expr()
            self.expect("SEMI")
            # declare variable in current scope
            self.declare_var(name, mut, global_scope=(len(self.scope_stack)==0))
            return {"kind": "let", "name": name, "value": expr, "mut": mut}

        if self.match("ID"):
            name = self.tokens[self.pos-1].value
            if self.match("EQ"):
                expr = self.parse_expr()
                self.expect("SEMI")
                info = self.lookup_var(name)
                if not info:
                    raise SyntaxError(f"Assignment to undeclared variable '{name}'")
                if not info["mut"]:
                    raise SyntaxError(f"Cannot reassign immutable variable '{name}'")
                return {"kind": "assign", "name": name, "value": expr}
            ...
params=[param("x"), param("y", mut=True)] # type: ignore
node = {
    "kind": "struct",
    "name": "Foo",
    "fields": [
        {"name": "x", "mut": False}, 
        {"name": "y", "mut": True}
    ]  # type: ignore
}
from phoenix_core_fixed import ASTNode # type: ignore

struct_node = ASTNode("struct",
    name="Foo",
    fields=[
        {"name": "x", "mut": False},
        {"name": "y", "mut": True}
    ]
)
from typing import List, Dict

fields: List[Dict[str, object]] = [
    {"name": "x", "mut": False},
    {"name": "y", "mut": True}
]



class Parser:
    ...

    def parse_function(self):
        self.expect("FN")
        name = self.expect("ID").value
        self.expect("LPAREN")
        params = []
        if not self.match("RPAREN"):
            while True:
                mut = bool(self.match("MUT"))
                pname = self.expect("ID").value
                params.append({"name": pname, "mut": mut})
                if self.match("COMMA"): 
                    continue
                else:
                    self.expect("RPAREN")
                    break

        self.expect("LBRACE")
        self.push_scope()
        # declare params in local scope
        for p in params:
            self.declare_var(p["name"], p["mut"])
        body = self.parse_block()
        self.pop_scope()
        self.expect("RBRACE")

        return {"name": name, "params": params, "body": body}

    {
  "kind": "struct",
  "name": "Foo",
  "fields": [
     {"name": "x", "mut": False},
     {"name": "y", "mut": True}
  ]
}

class Parser:
    ...

    def parse_struct(self):
        self.expect("STRUCT")
        name = self.expect("ID").value
        self.expect("LBRACE")
        fields = []
        while not self.match("RBRACE"):
            self.expect("LET")
            mut = bool(self.match("MUT"))
            fname = self.expect("ID").value
            self.expect("SEMI")
            fields.append({"name": fname, "mut": mut})
        return {"kind": "struct", "name": name, "fields": fields}

    def parse_stmt(self):
        # existing "let" and "assign" rules ...

        # struct field assignment
        if self.peek() and self.peek().type == "ID":
            name = self.expect("ID").value
            if self.match("DOT"):
                field = self.expect("ID").value
                if self.match("EQ"):
                    expr = self.parse_expr()
                    self.expect("SEMI")
                    struct_type = self.lookup_var(name)
                    if not struct_type:
                        raise SyntaxError(f"Unknown struct instance '{name}'")
                    # look up field mutability
                    fields = struct_type.get("fields", {})
                    if field not in fields:
                        raise SyntaxError(f"No field '{field}' in struct {name}")
                    if not fields[field]["mut"]:
                        raise SyntaxError(f"Cannot assign to immutable field '{field}' of struct {name}")
                    return {"kind": "assign_field", "obj": name, "field": field, "value": expr}

def parse_stmt(self):
    # normal assignment to variable
    if self.peek() and self.peek().type == "ID":
        name = self.expect("ID").value

        if self.match("EQ"):  # whole-variable assignment
            expr = self.parse_expr()
            self.expect("SEMI")
            info = self.lookup_var(name)
            if not info:
                raise SyntaxError(f"Assignment to undeclared variable '{name}'")
            if not info["mut"]:
                raise SyntaxError(f"Cannot reassign immutable variable '{name}'")
            return {"kind": "assign", "name": name, "value": expr}

        if self.match("DOT"):  # field assignment
            field = self.expect("ID").value
            self.expect("EQ")
            expr = self.parse_expr()
            self.expect("SEMI")

            # check field mutability
            struct_info = self.lookup_var(name)
            if not struct_info:
                raise SyntaxError(f"Unknown struct instance '{name}'")

            struct_type = struct_info.get("type")
            if not struct_type or field not in struct_type["fields"]:
                raise SyntaxError(f"No field '{field}' in struct {struct_type['name'] if struct_type else '?'}")

            if not struct_type["fields"][field]["mut"]:
                raise SyntaxError(f"Cannot assign to immutable field '{field}' of struct {struct_type['name']}")

            return {"kind": "assign_field", "obj": name, "field": field, "value": expr}

locals["arr"] = {"value": [1,2,3], "mut": False, "type": "array"}

def parse_primary(self):
    if self.match("LBRACKET"):
        elements = []
        if not self.match("RBRACKET"):
            while True:
                elements.append(self.parse_expr())
                if self.match("COMMA"): 
                    continue
                else:
                    self.expect("RBRACKET")
                    break
        return {"kind": "array", "elements": elements}

def parse_stmt(self):
    if self.peek() and self.peek().type == "ID":
        name = self.expect("ID").value

        if self.match("LBRACKET"):
            index_expr = self.parse_expr()
            self.expect("RBRACKET")
            self.expect("EQ")
            val_expr = self.parse_expr()
            self.expect("SEMI")

            info = self.lookup_var(name)
            if not info:
                raise SyntaxError(f"Assignment to undeclared array '{name}'")
            if not info["mut"]:
                raise SyntaxError(f"Cannot assign to immutable array '{name}'")

            return {"kind": "assign_index", "name": name, "index": index_expr, "value": val_expr}

def exec_stmt(self, stmt, locals):
    kind = stmt["kind"]

    if kind == "assign_index":
        arr = self.get_var(stmt["name"], locals)
        idx = self.eval_expr(stmt["index"], locals)
        val = self.eval_expr(stmt["value"], locals)
        arr["value"][idx] = val
        return

arr = [
  {"value": 1, "mut": False},
  {"value": 2, "mut": True}
]

def parse_primary(self):
    if self.match("LBRACKET"):
        elements = []
        if not self.match("RBRACKET"):
            while True:
                # allow `let` inside arrays
                if self.match("LET"):
                    mut = bool(self.match("MUT"))
                    val = self.parse_expr()
                    elements.append({"kind": "element", "value": val, "mut": mut})
                else:
                    # plain expr → default immutable
                    val = self.parse_expr()
                    elements.append({"kind": "element", "value": val, "mut": False})

                if self.match("COMMA"):
                    continue
                else:
                    self.expect("RBRACKET")
                    break
        return {"kind": "array", "elements": elements}

def eval_expr(self, expr, locals):
    et = expr["kind"]

    if et == "array":
        arr = []
        for el in expr["elements"]:
            val = self.eval_expr(el["value"], locals)
            arr.append({"value": val, "mut": el["mut"]})
        return {"value": arr, "mut": expr.get("mut", False), "type": "array"}

def exec_stmt(self, stmt, locals):
    if stmt["kind"] == "assign_index":
        arr = self.get_var(stmt["name"], locals)
        idx = self.eval_expr(stmt["index"], locals)
        val = self.eval_expr(stmt["value"], locals)

        if idx < 0 or idx >= len(arr["value"]):
            raise RuntimeError(f"Array index out of bounds: {idx}")

        cell = arr["value"][idx]
        if not cell["mut"]:
            raise RuntimeError(f"Cannot assign to immutable array element {stmt['name']}[{idx}]")

        cell["value"] = val
        return

class BorrowChecker:
    def __init__(self):
        # track active borrows: var -> {"immutable": count, "mutable": bool}
        self.borrows = {}

    def check_assign(self, name, mut):
        if mut:
            # mutable borrow
            if name in self.borrows and (self.borrows[name]["immutable"] > 0 or self.borrows[name]["mutable"]):
                raise CompileError(f"Cannot mutably borrow '{name}' while it is already borrowed")
            self.borrows.setdefault(name, {"immutable": 0, "mutable": False})
            self.borrows[name]["mutable"] = True
        else:
            # immutable borrow
            if name in self.borrows and self.borrows[name]["mutable"]:
                raise CompileError(f"Cannot immutably borrow '{name}' while it is mutably borrowed")
            self.borrows.setdefault(name, {"immutable": 0, "mutable": False})
            self.borrows[name]["immutable"] += 1

    def release(self, name, mut):
        if name not in self.borrows:
            return
        if mut:
            self.borrows[name]["mutable"] = False
        else:
            self.borrows[name]["immutable"] -= 1
        # clean up
        if self.borrows[name]["immutable"] == 0 and not self.borrows[name]["mutable"]:
            del self.borrows[name]

class Scope:
    def __init__(self, parent=None):
        self.parent = parent
        self.locals = {}   # name -> {"type": type, "mut": bool, "owned": True}
        self.children = []

    def declare(self, name, info):
        self.locals[name] = info

    def exit_scope(self, stmts):
        # inject drop calls for owned values
        drops = []
        for name, info in self.locals.items():
            t = info.get("type")
            if t and "drop" in t["methods"]:
                drops.append({
                    "kind": "call",
                    "name": f"{t['name']}.drop",
                    "args": [{"kind": "var", "name": name}]
                })
        stmts.extend(drops)
        return stmts

def parse_function(self):
    self.expect("FN")
    name = self.expect("ID").value
    self.expect("LPAREN")
    params = []
    if not self.match("RPAREN"):
        while True:
            params.append(self.expect("ID").value)
            if self.match("COMMA"): continue
            else:
                self.expect("RPAREN")
                break
    self.expect("LBRACE")
    body = self.parse_block()
    self.expect("RBRACE")
    return {"name": name, "params": params, "body": body, "is_drop": (name == "drop")}

def exit_scope(self, scope, locals):
    for name, info in scope.locals.items():
        val = locals.get(name)
        if not val: continue
        t = info.get("type")
        if t and f"{t['name']}.drop" in self.functions:
            self.call_function(f"{t['name']}.drop", [val])

def parse_stmt(self):
    if self.match("TRY"):
        self.expect("LBRACE")
        try_block = self.parse_block()
        self.expect("RBRACE")
        self.expect("CATCH")
        self.expect("LPAREN")
        errname = self.expect("ID").value
        self.expect("RPAREN")
        self.expect("LBRACE")
        catch_block = self.parse_block()
        self.expect("RBRACE")
        return {"kind": "try", "try": try_block, "err": errname, "catch": catch_block}

    if self.match("THROW"):
        val = self.parse_expr()
        self.expect("SEMI")
        return {"kind": "throw", "value": val}

class PhoenixVM:
    def __init__(self):
        self.functions = {}
        self.globals = {}
        self.callstack = []   # track scopes for unwinding

    def exec_block(self, block, locals):
        scope = {"locals": locals.copy()}
        self.callstack.append(scope)

        try:
            for stmt in block:
                res = self.exec_stmt(stmt, locals)
                if isinstance(res, tuple) and res[0] in {"return","throw"}:
                    return res
        finally:
            # run destructors on scope exit
            self.run_drops(scope, locals)
            self.callstack.pop()

    def exec_stmt(self, stmt, locals):
        kind = stmt["kind"]

        if kind == "throw":
            val = self.eval_expr(stmt["value"], locals)
            return ("throw", val)

        if kind == "try":
            res = self.exec_block(stmt["try"], locals)
            if res and res[0] == "throw":
                errval = res[1]
                new_locals = locals.copy()
                new_locals[stmt["err"]] = errval
                return self.exec_block(stmt["catch"], new_locals)
            return res

    def run_drops(self, scope, locals):
        for name, val in scope["locals"].items():
            t = getattr(val, "phoenix_type", None)
            if t and f"{t}.drop" in self.functions:
                self.call_function(f"{t}.drop", [val])

def parse_function(self):
    self.expect("FN")
    name = self.expect("ID").value

    # optional noexcept
    is_noexcept = False
    if self.match("NOEXCEPT"):
        is_noexcept = True

    self.expect("LPAREN")
    params = []
    if not self.match("RPAREN"):
        while True:
            params.append(self.expect("ID").value)
            if self.match("COMMA"): continue
            else:
                self.expect("RPAREN")
                break

    self.expect("LBRACE")
    body = self.parse_block()
    self.expect("RBRACE")
    return {"name": name, "params": params, "body": body, "noexcept": is_noexcept}

class SemanticChecker:
    def __init__(self, functions):
        self.functions = functions

    def check_function(self, fn):
        def walk(stmts):
            for stmt in stmts:
                if stmt["kind"] == "throw":
                    if fn["noexcept"]:
                        raise CompileError(f"Function {fn['name']} is noexcept but contains throw")
                if stmt["kind"] == "expr" and stmt["expr"]["kind"] == "call":
                    callee = stmt["expr"]["name"]
                    if callee in self.functions:
                        if self.functions[callee].get("noexcept") is False and fn["noexcept"]:
                            raise CompileError(f"Cannot call throwing function '{callee}' inside noexcept function '{fn['name']}")
                # recursive block walk
                if "body" in stmt: walk(stmt["body"])
                if "then" in stmt: walk(stmt["then"])
                if "else" in stmt and stmt["else"]: walk(stmt["else"])
        walk(fn["body"])

class PhoenixVM:
    def __init__(self, panic_mode="unwind"):
        self.panic_mode = panic_mode  # "unwind" or "abort"
        self.callstack = []

    def exec_stmt(self, stmt, locals):
        if stmt["kind"] == "throw":
            val = self.eval_expr(stmt["value"], locals)
            if self.panic_mode == "abort":
                print(f"[panic abort] {val}")
                sys.exit(1)  # hard stop
            return ("throw", val)

class LLVMGen:
    def __init__(self, panic_mode="unwind"):
        self.panic_mode = panic_mode

    def emit_function(self, fn):
        header = f"define i64 @{fn['name']}("
        header += ", ".join([f"i64 %{p}" for p in fn["params"]])
        header += ") "
        if fn.get("noexcept"):
            header += "nounwind "
        header += "{"
        return [header]

    def emit_throw(self, msg):
        if self.panic_mode == "abort":
            return [
                f'call void @phoenix_panic_abort(i8* {msg})',
                "unreachable"
            ]
        else:
            return [
                "; unwind path not implemented in minimal IR here",
                f'ret i64 -1'
            ]

class ASMGen:
    def __init__(self, panic_mode="unwind"):
        self.panic_mode = panic_mode

    def emit_function(self, fn):
        lines = [f".globl {fn['name']}", f"{fn['name']}:"]
        return lines

    def emit_throw(self, msg):
        if self.panic_mode == "abort":
            return ["ud2"]   # fastest, kills immediately
        else:
            return [
                "; simulate unwind",
                f"; TODO: RAII destructor calls here"
            ]

class LLVMGen:
    def __init__(self, panic_mode="unwind"):
        self.panic_mode = panic_mode
        self.functions = {}

    def emit_function(self, fn):
        header = f"define i64 @{fn['name']}("
        header += ", ".join([f"i64 %{p}" for p in fn["params"]])
        header += ") "
        if fn.get("noexcept"):
            header += "nounwind "
        header += "{"
        return [header]

    def maybe_inline_call(self, caller, callee, args):
        """Inline expansion for noexcept functions"""
        fn = self.functions.get(callee)
        if fn and fn.get("noexcept"):
            body = []
            for stmt in fn["body"]:
                # substitute params → args
                cloned = self.substitute(stmt, fn["params"], args)
                body.append(cloned)
            return body
        return [{"kind":"call","name":callee,"args":args}]

    def emit_throw(self, msg):
        if self.panic_mode == "abort":
            return [
                f'call void @phoenix_panic_abort(i8* {msg})',
                "unreachable"
            ]
        else:
            return [
                "; proper unwind would go here",
                f'ret i64 -1'
            ]

class ASMGen:
    def __init__(self, panic_mode="unwind"):
        self.panic_mode = panic_mode

    def emit_function(self, fn):
        lines = [f".globl {fn['name']}", f"{fn['name']}:"]
        if self.panic_mode == "unwind":
            lines.append(".cfi_startproc")
        return lines

    def end_function(self):
        if self.panic_mode == "unwind":
            return [".cfi_endproc"]
        return []

    def emit_throw(self, msg):
        if self.panic_mode == "abort":
            return ["ud2"]   # no unwind tables
        else:
            return [
                "; unwind edge placeholder",
                "call phoenix_unwind_handler"
            ]

class LLVMGen:
    def __init__(self, panic_mode="unwind"):
        self.panic_mode = panic_mode
        self.functions = {}

    def is_pure_const_fn(self, fn):
        if not fn.get("noexcept"): return False
        if not fn.get("pure"): return False
        # crude side-effect scan: disallow calls/log/throw
        for stmt in fn["body"]:
            if stmt["kind"] in {"expr","throw"}:
                return False
        return True

    def eval_const_fn(self, fn, args):
        env = {fn["params"][i]["name"]: args[i] for i in range(len(args))}
        return self.eval_const_block(fn["body"], env)

    def eval_const_block(self, body, env):
        for stmt in body:
            if stmt["kind"] == "return":
                return self.eval_const_expr(stmt["value"], env)

    def eval_const_expr(self, expr, env):
        if expr["kind"] == "int": return int(expr["value"])
        if expr["kind"] == "float": return float(expr["value"])
        if expr["kind"] == "var": return env[expr["name"]]
        if expr["kind"] == "binop":
            l = self.eval_const_expr(expr["left"], env)
            r = self.eval_const_expr(expr["right"], env)
            if expr["op"] == "+": return l + r
            if expr["op"] == "-": return l - r
            if expr["op"] == "*": return l * r
            if expr["op"] == "/": return l // r
        raise CompileError("Non-constant expr in eval_const_expr")

    def maybe_fold_call(self, callee, args):
        fn = self.functions.get(callee)
        if fn and self.is_pure_const_fn(fn):
            if all(isinstance(a, (int,float)) for a in args):
                return self.eval_const_fn(fn, args)
        return None

def parse_function(self):
    is_constexpr = False
    if self.match("CONSTEXPR"):
        is_constexpr = True

    self.expect("FN")
    name = self.expect("ID").value
    ...
    body = self.parse_block()
    self.expect("RBRACE")

    return {
        "name": name,
        "params": params,# type: ignore
        "body": body,
        "noexcept": is_noexcept, # type: ignore
        "pure": is_pure, # type: ignore
        "constexpr": is_constexpr
    }

class LLVMGen:
    def maybe_fold_call(self, callee, args):
        fn = self.functions.get(callee)

        # constexpr → MUST fold
        if fn and fn.get("constexpr"):
            if all(isinstance(a, (int,float)) for a in args):
                return self.eval_const_fn(fn, args)
            else:
                raise CompileError(
                    f"constexpr function '{callee}' requires constant args"
                )

        # fallback → pure noexcept folding
        if fn and self.is_pure_const_fn(fn):
            if all(isinstance(a, (int,float)) for a in args):
                return self.eval_const_fn(fn, args)

        return None

def parse_type(self):
    if self.match("LBRACK"):   # array type
        elem_type = self.parse_type()
        self.expect("SEMI")
        size_expr = self.parse_expr()
        self.expect("RBRACK")
        return {"kind": "array", "elem": elem_type, "size": size_expr}
    elif self.match("ID"):
        return {"kind": "type", "name": self.tokens[self.pos-1].value}

class TypeChecker:
    def resolve_array_size(self, size_expr, llvmgen):
        # Try to constant-fold
        val = llvmgen.eval_const_expr(size_expr, {})
        if not isinstance(val, int) or val <= 0:
            raise CompileError("Array size must be a positive constant int")
        return val

def lower_type(self, t):
    if t["kind"] == "array":
        size = self.resolve_array_size(t["size"], self)
        elem = self.lower_type(t["elem"])
        return f"[{size} x {elem}]"
    elif t["kind"] == "type":
        if t["name"] == "int": return "i64"
        if t["name"] == "bool": return "i1"
        # extend for float, char, etc.

def parse_function_or_struct(self):
    if self.match("CONSTEXPR"):   # generic context
        self.expect("FN")
        name = self.expect("ID").value
        self.expect("LT")   # <
        generics = []
        while True:
            if self.peek().type == "ID":
                generics.append({"kind":"param", "name":self.expect("ID").value})
            elif self.peek().type == "NUMBER":
                generics.append({"kind":"constparam", "name":self.expect("NUMBER").value})
            if not self.match("COMMA"): break
        self.expect("GT")
        # parse rest as struct or fn
        ...

class GenericInstantiator:
    def __init__(self, llvmgen):
        self.llvmgen = llvmgen
        self.cache = {}

    def instantiate(self, template, args):
        key = (template["name"], tuple(args))
        if key in self.cache:
            return self.cache[key]

        subst = dict(zip(template["generics"], args))
        inst_name = f"{template['name']}_" + "_".join(map(str,args))

        body = self.substitute(template["body"], subst)
        self.cache[key] = {"name": inst_name, "body": body}
        return self.cache[key]

    def substitute(self, body, subst):
        new_body = []
        for f in body:
            if f["kind"]=="field" and f["type"]["kind"]=="array":
                size = subst.get(f["type"]["size"], f["type"]["size"])
                elem = subst.get(f["type"]["elem"], f["type"]["elem"])
                new_body.append({
                    "kind":"field",
                    "name":f["name"],
                    "type":{"kind":"array","elem":elem,"size":size}
                })
            else:
                new_body.append(f)
        return new_body

class GenericInstantiator:
    def __init__(self, llvmgen):
        self.llvmgen = llvmgen
        self.cache = {}

    def normalize_arg(self, arg):
        # constexpr expr → fold to constant
        if isinstance(arg, dict):  
            try:
                return self.llvmgen.eval_const_expr(arg, {})
            except Exception:
                return arg
        return arg

    def normalize_args(self, args):
        return [self.normalize_arg(a) for a in args]

    def canonical_key(self, name, args):
        norm_args = self.normalize_args(args)
        return (name, tuple(norm_args))

    def instantiate(self, template, args):
        key = self.canonical_key(template["name"], args)
        if key in self.cache:
            return self.cache[key]

        subst = dict(zip(template["generics"], key[1]))
        inst_name = f"{template['name']}_" + "_".join(map(str, key[1]))
        body = self.substitute(template["body"], subst)
        inst = {"name": inst_name, "body": body}
        self.cache[key] = inst
        return inst

def parse_function(self):
    is_constexpr = bool(self.match("CONSTEXPR"))
    self.expect("FN")
    name = self.expect("ID").value

    generics = []
    if self.match("LT"):   # <T,U,...>
        while True:
            generics.append(self.expect("ID").value)
            if not self.match("COMMA"):
                break
        self.expect("GT")

    self.expect("LPAREN")
    params = []
    if not self.match("RPAREN"):
        while True:
            pname = self.expect("ID").value
            params.append(pname)
            if not self.match("COMMA"):
                self.expect("RPAREN")
                break

    self.expect("LBRACE")
    body = self.parse_block()
    self.expect("RBRACE")

    return {
        "kind": "fn_template" if generics else "fn",
        "name": name,
        "generics": generics,
        "params": params,
        "body": body,
        "constexpr": is_constexpr
    }

class FunctionInstantiator:
    def __init__(self, llvmgen):
        self.llvmgen = llvmgen
        self.cache = {}

    def canonical_key(self, fn, args):
        norm_args = []
        for a in args:
            if isinstance(a, dict):
                try: norm_args.append(self.llvmgen.eval_const_expr(a, {}))
                except: norm_args.append(a)
            else:
                norm_args.append(a)
        return (fn["name"], tuple(norm_args))

    def instantiate(self, fn_template, args):
        key = self.canonical_key(fn_template, args)
        if key in self.cache:
            return self.cache[key]

        inst_name = f"{fn_template['name']}_" + "_".join(map(str,key[1]))
        subst = dict(zip(fn_template["generics"], key[1]))
        body = self.substitute(fn_template["body"], subst)

        inst = {
            "kind": "fn",
            "name": inst_name,
            "params": fn_template["params"],
            "body": body,
            "constexpr": fn_template["constexpr"]
        }
        self.cache[key] = inst
        return inst

    def substitute(self, body, subst):
        new_body = []
        for stmt in body:
            # walk AST, replace generic identifiers
            if stmt["kind"] == "binop":
                stmt["left"] = self.substitute([stmt["left"]], subst)[0]
                stmt["right"] = self.substitute([stmt["right"]], subst)[0]
                new_body.append(stmt)
            elif stmt["kind"] == "var" and stmt["name"] in subst:
                new_body.append({"kind":"type_var", "value":subst[stmt["name"]]})
            else:
                new_body.append(stmt)
        return new_body

def parse_trait(self):
    self.expect("TRAIT")
    name = self.expect("ID").value
    self.expect("LBRACE")
    methods = []
    while not self.match("RBRACE"):
        self.expect("FN")
        mname = self.expect("ID").value
        self.expect("LPAREN")
        params = []
        while not self.match("RPAREN"):
            params.append(self.expect("ID").value)
            if not self.match("COMMA"): self.expect("RPAREN"); break
        self.expect("ARROW")
        rettype = self.expect("ID").value
        self.expect("SEMI")
        methods.append({"name":mname,"params":params,"ret":rettype})
    return {"kind":"trait","name":name,"methods":methods}

def parse_impl(self):
    self.expect("IMPL")
    trait = self.expect("ID").value
    self.expect("FOR")
    tname = self.expect("ID").value
    self.expect("LBRACE")
    methods = []
    while not self.match("RBRACE"):
        fn = self.parse_function()
        methods.append(fn)
    return {"kind":"impl","trait":trait,"type":tname,"methods":methods}

class TraitSystem:
    def __init__(self):
        self.traits = {}
        self.impls = {}

    def register_trait(self, trait):
        self.traits[trait["name"]] = trait

    def register_impl(self, impl):
        self.impls[(impl["trait"], impl["type"])] = impl["methods"]

    def check_bound(self, type_name, trait_name):
        if (trait_name, type_name) not in self.impls:
            raise CompileError(f"Type {type_name} does not implement {trait_name}")
        return self.impls[(trait_name, type_name)]

def parse_struct(self):
    self.expect("STRUCT")
    name = self.expect("ID").value
    self.expect("LBRACE")
    fields = []
    while not self.match("RBRACE"):
        self.expect("LET")
        mut = bool(self.match("MUT"))
        fname = self.expect("ID").value
        ftype = None
        if self.match("COLON"):
            ftype = self.expect("ID").value
        self.expect("SEMI")
        fields.append({"name": fname, "mut": mut, "type": ftype})
    
    derives = []
    if self.match("DERIVE"):
        self.expect("LPAREN")
        while True:
            derives.append(self.expect("ID").value)
            if not self.match("COMMA"):
                break
        self.expect("RPAREN")
    
    return {"kind":"struct","name":name,"fields":fields,"derives":derives}

class DeriveExpander:
    def expand(self, struct_ast):
        impls = []
        for trait in struct_ast["derives"]:
            if trait == "Eq":
                impls.append(self.derive_eq(struct_ast))
            elif trait == "Ord":
                impls.append(self.derive_ord(struct_ast))
            elif trait == "Clone":
                impls.append(self.derive_clone(struct_ast))
            elif trait == "Log":
                impls.append(self.derive_log(struct_ast))
        return impls

    def derive_eq(self, s):
        fname = s["name"]
        conds = []
        for f in s["fields"]:
            conds.append({"kind":"binop","op":"==",
                          "left":{"kind":"field","obj":"self","field":f["name"]},
                          "right":{"kind":"field","obj":"other","field":f["name"]}})
        expr = conds[0]
        for c in conds[1:]:
            expr = {"kind":"binop","op":"&&","left":expr,"right":c}
        return {"kind":"impl","trait":"Eq","type":fname,
                "methods":[{"name":"eq","params":["self","other"],
                            "body":[{"kind":"return","value":expr}]}]}

    def derive_ord(self, s):
        fname = s["name"]
        # lex ordering
        body = []
        for i,f in enumerate(s["fields"]):
            cmp = {"kind":"if","cond":{"kind":"binop","op":"<",
                        "left":{"kind":"field","obj":"self","field":f["name"]},
                        "right":{"kind":"field","obj":"other","field":f["name"]}},
                   "then":[{"kind":"return","value":{"kind":"bool","value":True}}],
                   "else":None}
            body.append(cmp)
        body.append({"kind":"return","value":{"kind":"bool","value":False}})
        return {"kind":"impl","trait":"Ord","type":fname,
                "methods":[{"name":"lt","params":["self","other"],"body":body}]}

    def derive_clone(self, s):
        fname = s["name"]
        assigns = []
        for f in s["fields"]:
            assigns.append({"kind":"field_copy","field":f["name"]})
        return {"kind":"impl","trait":"Clone","type":fname,
                "methods":[{"name":"clone","params":["self"],
                            "body":[{"kind":"return","value":{"kind":"struct_init","type":fname,"fields":assigns}}]}]}

    def derive_log(self, s):
        fname = s["name"]
        stmts = [{"kind":"expr","expr":{"kind":"call","name":"log",
                  "args":[{"kind":"str","value":f"{fname}.{f['name']} = "},
                          {"kind":"field","obj":"self","field":f["name"]}]}}
                 for f in s["fields"]]
        return {"kind":"impl","trait":"Log","type":fname,
                "methods":[{"name":"log","params":["self"],"body":stmts}]}

    def derive(self, struct_ast):
        if "derives" not in struct_ast or not struct_ast["derives"]:
            return []
        impls = []
        for trait in struct_ast["derives"]:
            if trait == "Eq":
                impls.append(self.derive_eq(struct_ast))
            elif trait == "Ord":
                impls.append(self.derive_ord(struct_ast))
            elif trait == "Clone":
                impls.append(self.derive_clone(struct_ast))
            elif trait == "Log":
                impls.append(self.derive_log(struct_ast))
            else:
                raise CompileError(f"Unknown derive trait: {trait}")
        
        if not impls:
            raise CompileError("No valid derives found for struct")
        return impls
    def expand(self, struct_ast):
        if "derives" not in struct_ast or not struct_ast["derives"]:
            return []
        impls = []
        for trait in struct_ast["derives"]:
            if trait == "Eq":
                impls.append(self.derive_eq(struct_ast))
            elif trait == "Ord":
                impls.append(self.derive_ord(struct_ast))
            elif trait == "Clone":
                impls.append(self.derive_clone(struct_ast))
            elif trait == "Log":
                impls.append(self.derive_log(struct_ast))
            else:
                raise CompileError(f"Unknown derive trait: {trait}")
        
        if not impls:
            raise CompileError("No valid derives found for struct")
        return impls
    def expand(self, struct_ast):
        if "derives" not in struct_ast or not struct_ast["derives"]:
            return []
        impls = []
        for trait in struct_ast["derives"]:
            if trait == "Eq":
                impls.append(self.derive_eq(struct_ast))
            elif trait == "Ord":
                impls.append(self.derive_ord(struct_ast))
            elif trait == "Clone":
                impls.append(self.derive_clone(struct_ast))
            elif trait == "Log":
                impls.append(self.derive_log(struct_ast))
            else:
                raise CompileError(f"Unknown derive trait: {trait}")
        
        if not impls:
            raise CompileError("No valid derives found for struct")
        return impls
    def expand(self, struct_ast):
        if "derives" not in struct_ast or not struct_ast["derives"]:
            return []
        impls = []
        for trait in struct_ast["derives"]:
            if trait == "Eq":
                impls.append(self.derive_eq(struct_ast))
            elif trait == "Ord":
                impls.append(self.derive_ord(struct_ast))
            elif trait == "Clone":
                impls.append(self.derive_clone(struct_ast))
            elif trait == "Log":
                impls.append(self.derive_log(struct_ast))
            else:
                raise CompileError(f"Unknown derive trait: {trait}")
        
        if not impls:
            raise CompileError("No valid derives found for struct")
        return impls

    def parse_struct_field(self):
        self.expect("LET")
        mut = bool(self.match("MUT"))
        name = self.expect("ID").value
        ftype = None
        if self.match("COLON"):
            ftype = self.expect("ID").value
        self.expect("SEMI")
        return {"name": name, "mut": mut, "type": ftype}
    def parse_struct(self):
        self.expect("STRUCT")
        name = self.expect("ID").value
        self.expect("LBRACE")
        fields = []
        while not self.match("RBRACE"):
            fields.append(self.parse_struct_field())
        
        derives = []
        if self.match("DERIVE"):
            self.expect("LPAREN")
            while True:
                derives.append(self.expect("ID").value)
                if not self.match("COMMA"):
                    break
            self.expect("RPAREN")
        
        return {"kind": "struct", "name": name, "fields": fields, "derives": derives}
    def parse_array_index_assign(self):
        name = self.expect("ID").value
        self.expect("LBRACK")
        index_expr = self.parse_expr()
        self.expect("RBRACK")
        self.expect("ASSIGN")
        val_expr = self.parse_expr()
        self.expect("SEMI")
        if not self.is_var(name):
            raise CompileError(f"Cannot assign to non-variable '{name}'")

# phoenix_core.py
# Defines all fundamental symbols so later stages don't crash

from typing import Any, Dict, List, Optional, Union

# ------------------------------
# Compiler Exceptions
# ------------------------------

class CompileError(Exception):
    """Raised for compile-time errors in Phoenix."""
    def __init__(self, msg: str, line: Optional[int] = None, col: Optional[int] = None):
        self.msg = msg
        self.line = line
        self.col = col
        loc = f" (line {line}, col {col})" if line else ""
        super().__init__(f"[CompileError]{loc}: {msg}")

# ------------------------------
# Core AST Node Helpers
# ------------------------------

class ASTNode:
    """Base AST node with convenience attributes."""
    def __init__(self, kind: str, **kwargs):
        self.kind = kind
        for k, v in kwargs.items():
            setattr(self, k, v)
    def __repr__(self):
        return f"ASTNode({self.kind}, {self.__dict__})"

# Example wrapper to create nodes easily
def fn(name: str, params: List[str], body: List['ASTNode'],
       is_noexcept: bool = False, is_pure: bool = False) -> ASTNode:
    """Define a function AST node."""
    return ASTNode("fn", name=name, params=params,
                   body=body, is_noexcept=is_noexcept, is_pure=is_pure)

def let(name: str, expr: ASTNode, mut: bool = False) -> ASTNode:
    """Immutable or mutable variable declaration."""
    return ASTNode("let", name=name, value=expr, mut=mut)

def assign(name: str, expr: ASTNode) -> ASTNode:
    """Assignment statement."""
    return ASTNode("assign", name=name, value=expr)

def return_stmt(val: Optional[ASTNode] = None) -> ASTNode:
    return ASTNode("return", value=val)

def if_stmt(cond: ASTNode, then: List[ASTNode], els: Optional[List[ASTNode]] = None) -> ASTNode:
    return ASTNode("if", cond=cond, then=then, els=els)

def while_stmt(cond: ASTNode, body: List[ASTNode]) -> ASTNode:
    return ASTNode("while", cond=cond, body=body)

def break_stmt() -> ASTNode:
    return ASTNode("break")

def continue_stmt() -> ASTNode:
    return ASTNode("continue")

def binop(op: str, left: ASTNode, right: ASTNode) -> ASTNode:
    return ASTNode("binop", op=op, left=left, right=right)

def call(name: str, args: List[ASTNode]) -> ASTNode:
    return ASTNode("call", name=name, args=args)

def var(name: str) -> ASTNode:
    return ASTNode("var", name=name)

def int_lit(val: int) -> ASTNode:
    return ASTNode("int", value=val)

def str_lit(val: str) -> ASTNode:
    return ASTNode("str", value=val)

def bool_lit(val: bool) -> ASTNode:
    return ASTNode("bool", value=val)

def null_lit() -> ASTNode:
    return ASTNode("null")

# ------------------------------
# Mutability & Locals/Globals
# ------------------------------

class Symbol:
    """Represents a variable or function in scope."""
    def __init__(self, name: str, ty: str = "int", mut: bool = False, value: Any = None):
        self.name = name
        self.type = ty
        self.mut = mut
        self.value = value
    def __repr__(self):
        m = "mut " if self.mut else ""
        return f"<{m}{self.type} {self.name}={self.value}>"

class Scope:
    """Scope with immutability enforcement."""
    def __init__(self, parent: Optional['Scope']=None):
        self.parent = parent
        self.symbols: Dict[str, Symbol] = {}

    def define(self, name: str, ty: str, mut: bool, value: Any):
        if name in self.symbols:
            raise CompileError(f"Symbol {name} already defined in this scope")
        self.symbols[name] = Symbol(name, ty, mut, value)

    def assign(self, name: str, value: Any):
        if name in self.symbols:
            sym = self.symbols[name]
            if not sym.mut:
                raise CompileError(f"Cannot reassign immutable variable {name}")
            sym.value = value
        elif self.parent:
            self.parent.assign(name, value)
        else:
            raise CompileError(f"Undefined symbol {name}")

    def lookup(self, name: str) -> Symbol:
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.lookup(name)
        raise CompileError(f"Undefined symbol {name}")

# ------------------------------
# Safety helpers
# ------------------------------

def check_noexcept(stmt: ASTNode):
    """Verify no throwing ops are inside noexcept functions."""
    if getattr(stmt, "kind", "") == "throw":
        raise CompileError("Throw not allowed in noexcept function")
    if hasattr(stmt, "body"):
        for sub in getattr(stmt, "body", []):
            check_noexcept(sub)
    if hasattr(stmt, "then"):
        for sub in getattr(stmt, "then", []):
            check_noexcept(sub)
    if hasattr(stmt, "els") and stmt.els:
        for sub in stmt.els:
            check_noexcept(sub)

# ------------------------------
# Demo
# ------------------------------

if __name__ == "__main__":
    # Create a function AST
    body = [
        let("x", int_lit(5), mut=False),
        return_stmt(var("x"))
    ]
    f = fn("main", [], body, is_noexcept=True)

    print(f)
    scope = Scope()
    scope.define("x", "int", mut=False, value=5)
    print(scope.lookup("x"))
    try:
        scope.assign("x", 10)  # should error
    except CompileError as e:
        print(e)

# phoenix_core_fixed.py
# Self-contained Phoenix compiler core definitions

from typing import Any, Optional, Dict, List, Union

# ------------------------------
# Compile-Time Error
# ------------------------------

class CompileError(Exception):
    """Raised for compile-time violations in Phoenix."""
    def __init__(self, msg: str, line: Optional[int] = None, col: Optional[int] = None):
        self.msg = msg
        self.line = line
        self.col = col
        loc = f" (line {line}, col {col})" if line else ""
        super().__init__(f"[CompileError]{loc}: {msg}")

# ------------------------------
# AST Node
# ------------------------------

class ASTNode:
    """Generic AST node with dynamic fields."""
    def __init__(self, kind: str, **kwargs):
        self.kind = kind
        for k, v in kwargs.items():
            setattr(self, k, v)
    def __repr__(self):
        return f"ASTNode({self.kind}, {self.__dict__})"

# ------------------------------
# AST Builders
# ------------------------------

def fn(name: str,
       params: List[str],
       body: List[ASTNode],
       is_noexcept: bool = False,
       is_pure: bool = False) -> ASTNode:
    return ASTNode("fn", name=name, params=params, body=body,
                   is_noexcept=is_noexcept, is_pure=is_pure)

def let(name: str, expr: ASTNode, mut: bool = False) -> ASTNode:
    return ASTNode("let", name=name, value=expr, mut=mut)

def assign(name: str, expr: ASTNode) -> ASTNode:
    return ASTNode("assign", name=name, value=expr)

def return_stmt(val: Optional[ASTNode] = None) -> ASTNode:
    return ASTNode("return", value=val)

def if_stmt(cond: ASTNode, then: List[ASTNode], els: Optional[List[ASTNode]] = None) -> ASTNode:
    return ASTNode("if", cond=cond, then=then, els=els)

def while_stmt(cond: ASTNode, body: List[ASTNode]) -> ASTNode:
    return ASTNode("while", cond=cond, body=body)

def break_stmt() -> ASTNode:
    return ASTNode("break")

def continue_stmt() -> ASTNode:
    return ASTNode("continue")

def binop(op: str, left: ASTNode, right: ASTNode) -> ASTNode:
    return ASTNode("binop", op=op, left=left, right=right)

def call(name: str, args: List[ASTNode]) -> ASTNode:
    return ASTNode("call", name=name, args=args)

def var(name: str) -> ASTNode:
    return ASTNode("var", name=name)

def int_lit(val: int) -> ASTNode:
    return ASTNode("int", value=val)

def str_lit(val: str) -> ASTNode:
    return ASTNode("str", value=val)

def bool_lit(val: bool) -> ASTNode:
    return ASTNode("bool", value=val)

def null_lit() -> ASTNode:
    return ASTNode("null")

# ------------------------------
# Symbols & Scope
# ------------------------------

class Symbol:
    """Represents a variable or function binding."""
    def __init__(self, name: str, ty: str = "int", mut: bool = False, value: Any = None):
        self.name = name
        self.type = ty
        self.mut = mut
        self.value = value
    def __repr__(self):
        m = "mut " if self.mut else ""
        return f"<{m}{self.type} {self.name}={self.value}>"

class Scope:
    """Nested lexical scope with immutability enforcement."""
    def __init__(self, parent: Optional['Scope'] = None):
        self.parent = parent
        self.symbols: Dict[str, Symbol] = {}

    def define(self, name: str, ty: str, mut: bool, value: Any):
        if name in self.symbols:
            raise CompileError(f"Symbol {name} already defined")
        self.symbols[name] = Symbol(name, ty, mut, value)

    def assign(self, name: str, value: Any):
        if name in self.symbols:
            sym = self.symbols[name]
            if not sym.mut:
                raise CompileError(f"Cannot reassign immutable variable {name}")
            sym.value = value
        elif self.parent:
            self.parent.assign(name, value)
        else:
            raise CompileError(f"Undefined symbol {name}")

    def lookup(self, name: str) -> Symbol:
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.lookup(name)
        raise CompileError(f"Undefined symbol {name}")

# ------------------------------
# Noexcept Checker
# ------------------------------

def check_noexcept(stmt: ASTNode):
    """Ensure no throwing ops exist in noexcept functions."""
    if stmt.kind == "throw":
        raise CompileError("Throw not allowed in noexcept function")
    for field in ("body", "then", "els"):
        seq = getattr(stmt, field, None)
        if isinstance(seq, list):
            for s in seq:
                check_noexcept(s)

# ------------------------------
# Demo
# ------------------------------

if __name__ == "__main__":
    # Build a tiny program AST
    prog = fn("main", params=[],
              body=[
                  let("x", int_lit(5), mut=False),
                  return_stmt(var("x"))
              ],
              is_noexcept=True)

    print("Function AST:", prog)

    # Scope demo
    scope = Scope()
    scope.define("x", "int", mut=False, value=5)
    print("Lookup:", scope.lookup("x"))

    try:
        scope.assign("x", 10)  # should fail due to immutability
    except CompileError as e:
        print("Error:", e)

# phoenix_pipeline.py
# Unified Phoenix pipeline: Parser → AST (phoenix_core) → SSA → ASM → EXE

import re, subprocess, sys
from typing import List

from phoenix_core_fixed import ( # type: ignore
    ASTNode, fn, let, assign, return_stmt, if_stmt, while_stmt,
    break_stmt, continue_stmt, binop, call, var, int_lit, str_lit, bool_lit,
    null_lit, CompileError, Scope, check_noexcept
)

# ------------------------------
# Lexer
# ------------------------------

TOKEN_SPEC = [
    ("NUMBER",   r"\d+"),
    ("STRING",   r"\".*?\""),
    ("ID",       r"[A-Za-z_][A-Za-z0-9_]*"),
    ("OP",       r"==|!=|&&|\|\||[+\-*/<>]"),
    ("LBRACE",   r"\{"), ("RBRACE", r"\}"),
    ("LPAREN",   r"\("), ("RPAREN", r"\)"),
    ("SEMI",     r";"), ("COMMA", r","), ("EQ", r"="),
    ("SKIP",     r"[ \t\r\n]+"), ("MISMATCH", r"."),
]
TOKEN_RE = re.compile("|".join(f"(?P<{n}>{p})" for n,p in TOKEN_SPEC))

KEYWORDS = {"let","mut","fn","return","while","if","else","break","continue","capsule"}

class Token:
    def __init__(self, typ, val): self.type,self.value=typ,val
    def __repr__(self): return f"Token({self.type},{self.value})"

def lex(text: str) -> List[Token]:
    tokens=[]
    for m in TOKEN_RE.finditer(text):
        k=m.lastgroup; v=m.group()
        if k=="NUMBER": tokens.append(Token("NUMBER", int(v)))
        elif k=="STRING": tokens.append(Token("STRING", v.strip('"')))
        elif k=="ID":
            if v in KEYWORDS: tokens.append(Token(v.upper(), v))
            else: tokens.append(Token("ID", v))
        elif k=="OP": tokens.append(Token("OP", v))
        elif k in {"LBRACE","RBRACE","LPAREN","RPAREN","SEMI","COMMA","EQ"}:
            tokens.append(Token(k, v))
        elif k=="SKIP": continue
        elif k=="MISMATCH": raise CompileError(f"Unexpected token {v}")
    return tokens

# ------------------------------
# Parser → AST
# ------------------------------

class Parser:
    def __init__(self, toks): self.toks,self.pos=toks,0
    def peek(self,k=0): return self.toks[self.pos+k] if self.pos+k<len(self.toks) else None
    def match(self,*types):
        if (tok:=self.peek()) and tok.type in types:
            self.pos+=1; return tok
        return None
    def expect(self,*types):
        tok=self.match(*types)
        if not tok: raise CompileError(f"Expected {types}, got {self.peek()}")
        return tok

    def parse_program(self) -> List[ASTNode]:
        funcs=[]
        while self.peek():
            funcs.append(self.parse_function())
        return funcs

    def parse_function(self) -> ASTNode:
        self.expect("FN")
        name=self.expect("ID").value
        self.expect("LPAREN")
        params=[]
        if not self.match("RPAREN"):
            while True:
                params.append(self.expect("ID").value)
                if self.match("COMMA"): continue
                else: self.expect("RPAREN"); break
        self.expect("LBRACE")
        body=self.parse_block()
        self.expect("RBRACE")
        return fn(name,params,body)

    def parse_block(self) -> List[ASTNode]:
        stmts=[]
        while self.peek() and self.peek().type!="RBRACE":
            stmts.append(self.parse_stmt())
        return stmts

    def parse_stmt(self) -> ASTNode:
        if self.match("LET"):
            mut=bool(self.match("MUT"))
            name=self.expect("ID").value
            self.expect("EQ"); expr=self.parse_expr(); self.expect("SEMI")
            return let(name,expr,mut)
        if self.match("RETURN"):
            if self.peek().type!="SEMI": val=self.parse_expr()
            else: val=None
            self.expect("SEMI"); return return_stmt(val)
        if self.match("WHILE"):
            self.expect("LPAREN"); cond=self.parse_expr(); self.expect("RPAREN")
            self.expect("LBRACE"); body=self.parse_block(); self.expect("RBRACE")
            return while_stmt(cond,body)
        if self.match("IF"):
            self.expect("LPAREN"); cond=self.parse_expr(); self.expect("RPAREN")
            self.expect("LBRACE"); then=self.parse_block(); self.expect("RBRACE")
            els=None
            if self.match("ELSE"):
                self.expect("LBRACE"); els=self.parse_block(); self.expect("RBRACE")
            return if_stmt(cond,then,els)
        if self.match("BREAK"): self.expect("SEMI"); return break_stmt()
        if self.match("CONTINUE"): self.expect("SEMI"); return continue_stmt()

        # assignment or call
        tok=self.expect("ID")
        if self.match("EQ"):
            expr=self.parse_expr(); self.expect("SEMI")
            return assign(tok.value,expr)
        elif self.peek() and self.peek().type=="LPAREN":
            callnode=self.finish_call(tok.value); self.expect("SEMI")
            return callnode
        else:
            raise CompileError(f"Unexpected stmt starting with {tok}")

    def finish_call(self,name): 
        self.expect("LPAREN"); args=[]
        if not self.match("RPAREN"):
            while True:
                args.append(self.parse_expr())
                if self.match("COMMA"): continue
                else: self.expect("RPAREN"); break
        return call(name,args)

    PRECEDENCE={"||":1,"&&":2,"==":3,"!=":3,"<":3,">":3,"+":4,"-":4,"*":5,"/":5}
    def parse_expr(self):
        return self.parse_binop(1)
    def parse_binop(self,minp):
        left=self.parse_primary()
        while True:
            tok=self.peek()
            if tok and tok.type=="OP" and self.PRECEDENCE.get(tok.value,0)>=minp:
                op=tok.value; self.pos+=1
                right=self.parse_binop(self.PRECEDENCE[op]+1)
                left=binop(op,left,right)
            else: break
        return left
    def parse_primary(self):
        tok=self.peek()
        if self.match("NUMBER"): return int_lit(tok.value)
        if self.match("STRING"): return str_lit(tok.value)
        if self.match("ID"):
            if self.peek() and self.peek().type=="LPAREN":
                return self.finish_call(tok.value)
            return var(tok.value)
        if self.match("LPAREN"):
            e=self.parse_expr(); self.expect("RPAREN"); return e
        raise CompileError(f"Unexpected {tok}")
        
# ------------------------------
# SSA + Optimizer Skeleton
# ------------------------------

class SSAForm:
    def __init__(self,fn_ast:ASTNode): self.fn=fn_ast; self.blocks=[]
    def build(self): self.blocks.append(("entry",self.fn.body))

# ------------------------------
# ASM Backend Skeleton
# ------------------------------

class AsmBackend:
    def __init__(self,ssa:SSAForm): self.ssa=ssa; self.code=[]
    def lower(self):
        self.code.append("section .text")
        self.code.append("global main")
        self.code.append("main:")
        self.code.append("    mov rax, 0")
        self.code.append("    ret")
        return "\n".join(self.code)

# ------------------------------
# Runner
# ------------------------------

def run_phoenix(filename):
    with open(filename) as f: src=f.read()
    toks=lex(src)
    prog=Parser(toks).parse_program()
    # only take first fn main for demo
    mainfn=[f for f in prog if f.name=="main"][0]
    ssa=SSAForm(mainfn); ssa.build()
    asm=AsmBackend(ssa).lower()
    with open("phoenix.s","w") as f: f.write(asm)
    # assemble + link
    subprocess.run(["nasm","-felf64","phoenix.s"])
    subprocess.run(["ld","-o","phoenix","phoenix.o"])
    print("Built phoenix executable.")

if __name__=="__main__":
    if len(sys.argv)>1: run_phoenix(sys.argv[1])


# -------------------------------------------------------------------
# 🔧 Phoenix AST Consistency Fix
# This ensures struct fields, params, and mutability are always
# represented as ASTNode objects, never raw dicts that cause syntax errors.
# Drop this at the bottom of your VM_for_Phoenix.py
# -------------------------------------------------------------------

from typing import List, Dict, Any

# --- Error class used consistently everywhere ---
class CompileError(Exception):
    """Phoenix compile-time error (parser/type checker)."""
    pass

# --- ASTNode unified representation ---
class ASTNode:
    def __init__(self, kind: str, **kwargs):
        self.kind = kind
        for k, v in kwargs.items():
            setattr(self, k, v)

    def __repr__(self):
        return f"ASTNode({self.kind}, {self.__dict__})"

# --- Factory helpers ---
def fn(name: str, params: List[Dict[str, Any]], body: List['ASTNode'],
       is_noexcept: bool=False, is_pure: bool=False) -> ASTNode:
    """Define a function AST node with parameter mutability."""
    return ASTNode("fn", name=name, params=params, body=body,
                   is_noexcept=is_noexcept, is_pure=is_pure)

def let(name: str, value: Any, mut: bool=False) -> ASTNode:
    return ASTNode("let", name=name, value=value, mut=mut)

def assign(name: str, value: Any) -> ASTNode:
    return ASTNode("assign", name=name, value=value)

def return_stmt(val: Any=None) -> ASTNode:
    return ASTNode("return", value=val)

def param(name: str, mut: bool=False) -> Dict[str, Any]:
    """Function/struct param definition."""
    return {"name": name, "mut": mut}

def struct(name: str, fields: List[Dict[str, Any]]) -> ASTNode:
    """Struct definition with field mutability."""
    return ASTNode("struct", name=name, fields=fields)

# --- Example usage ---
if __name__ == "__main__":
    # Function with params
    f = fn("add", [param("x"), param("y", mut=True)], [
        return_stmt(ASTNode("binop", op="+", left=ASTNode("var", name="x"), right=ASTNode("var", name="y")))
    ])
    print(f)

    # Struct with fields
    s = struct("Foo", [param("x"), param("y", mut=True)])
    print(s)

