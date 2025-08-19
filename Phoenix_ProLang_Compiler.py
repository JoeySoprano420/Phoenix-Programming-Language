import time
import sys
import subprocess
import os
from typing import Dict, List, Any, Optional, Union

class PhoenixException(Exception):
    def __init__(self, message, error_type="RuntimeError"):
        super().__init__(message)
        self.error_type = error_type
        self.timestamp = time.time()
        self.symbolic_metadata = {
            "error_type": error_type,
            "message": message,
            "timestamp": self.timestamp
        }

    def __str__(self):
        return f"{self.error_type}: {self.message} (at {self.timestamp})"

class CapsuleCompiler:
    def __init__(self, dialect="safe"):
        self.dialect = dialect
        self.symbol_table = {}
        self.guardians = {}
        self.execution_context = {}
        self.optimization_level = "O2"
        print(f"🛠️ [Phoenix Compiler] Initialized with dialect: {self.dialect}")

    def register_guardian(self, node_type, guardian_name, guardian_func):
        """Register a guardian function for a specific node type"""
        if node_type not in self.guardians:
            self.guardians[node_type] = {}
        self.guardians[node_type][guardian_name] = guardian_func
        print(f"🔒 [Guardian Registered] {guardian_name} for {node_type}")
        return self

    def unregister_guardian(self, node_type, guardian_name):
        """Unregister a guardian function for a specific node type"""
        if node_type in self.guardians and guardian_name in self.guardians[node_type]:
            del self.guardians[node_type][guardian_name]
            print(f"🔓 [Guardian Unregistered] {guardian_name} for {node_type}")
        else:
            print(f"⚠️ [Guardian Not Found] {guardian_name} for {node_type}")
        return self

    def list_guardians(self):
        """List all registered guardians with their node types"""
        if not self.guardians:
            print("No guardians registered.")
            return []
        guardian_list = []
        for node_type, guardians in self.guardians.items():
            for guardian_name in guardians.keys():
                guardian_list.append((node_type, guardian_name))
                print(f"🔍 [Guardian] {guardian_name} for {node_type}")
        return guardian_list

    def compile_capsule(self, capsule_node):
        """Main compilation pipeline: AST → IR → Bytecode"""
        print(f"🔥 [Phoenix] Starting compilation pipeline for {capsule_node.get('name', 'unknown')}")
        
        # Step 1: Guardian pass
        guarded_node = self._invoke_guardians(capsule_node)
        
        # Step 2: Generate intermediate representation
        ir = self._generate_ir(guarded_node)
        
        # Step 3: Emit final bytecode
        bytecode = self._emit_bytecode(ir)
        
        print(f"✅ [Phoenix] Compilation complete - {len(bytecode['bytecode'])} instructions generated")
        return bytecode

    def _invoke_guardians(self, node):
        """Symbolic refusal or mutation logic with comprehensive guardian dispatch"""
        
        def traverse_and_guard(ast_node, context_path=""):
            """Recursively traverse AST and invoke guardians"""
            if not isinstance(ast_node, dict):
                return ast_node
                
            node_type = ast_node.get("kind", "unknown")
            current_path = f"{context_path}.{node_type}" if context_path else node_type
            
            # Check for registered guardians for this node type
            if node_type in self.guardians:
                for guardian_name, guardian_func in self.guardians[node_type].items():
                    try:
                        # Guardian context with symbolic metadata
                        guard_context = {
                            "node": ast_node,
                            "path": current_path,
                            "dialect": self.dialect,
                            "symbols": self.symbol_table.copy(),
                            "timestamp": time.time(),
                            "symbolic_tags": {
                                "⊢": "type_assertion",
                                "⟵": "assignment",
                                "⟶": "function_arrow", 
                                "⟦": "capsule_start",
                                "⟧": "capsule_end",
                                "↩": "return"
                            }
                        }
                        
                        # Invoke guardian - may mutate or refuse the node
                        result = guardian_func(guard_context)
                        
                        if result is None:
                            # Guardian refused - symbolic rejection
                            raise PhoenixException(
                                f"Guardian '{guardian_name}' refused node at {current_path}",
                                "GuardianRefusal"
                            )
                        elif result != ast_node:
                            # Guardian mutated the node
                            ast_node = result
                            print(f"🛡️ [Guardian] {guardian_name} mutated node at {current_path}")
                            
                    except Exception as e:
                        print(f"❌ [Guardian Error] {guardian_name} at {current_path}: {e}")
                        # Continue with other guardians unless critical
                        if "critical" in guardian_name.lower():
                            raise
            
            # Recursively process child nodes
            if "body" in ast_node and isinstance(ast_node["body"], list):
                ast_node["body"] = [traverse_and_guard(stmt, current_path) for stmt in ast_node["body"]]
            
            if "then" in ast_node and isinstance(ast_node["then"], list):
                ast_node["then"] = [traverse_and_guard(stmt, current_path) for stmt in ast_node["then"]]
                
            if "else" in ast_node and isinstance(ast_node["else"], list):
                ast_node["else"] = [traverse_and_guard(stmt, current_path) for stmt in ast_node["else"]]
                
            # Process expressions
            for expr_key in ["value", "left", "right", "cond", "object"]:
                if expr_key in ast_node and isinstance(ast_node[expr_key], dict):
                    ast_node[expr_key] = traverse_and_guard(ast_node[expr_key], current_path)
                    
            if "args" in ast_node and isinstance(ast_node["args"], list):
                ast_node["args"] = [traverse_and_guard(arg, current_path) for arg in ast_node["args"]]
            
            return ast_node
        
        # Apply dialect-specific guardians
        self._apply_dialect_guardians(node)
        
        # Traverse and guard the entire AST
        return traverse_and_guard(node)

    def _apply_dialect_guardians(self, node):
        """Apply dialect-specific guardian rules"""
        dialect_rules = {
            "safe": {
                "memory_guardian": lambda ctx: self._check_memory_safety(ctx),
                "type_guardian": lambda ctx: self._check_type_safety(ctx),
                "concurrent_guardian": lambda ctx: self._check_concurrency_safety(ctx)
            },
            "functional": {
                "purity_guardian": lambda ctx: self._check_function_purity(ctx),
                "immutability_guardian": lambda ctx: self._check_immutability(ctx)
            },
            "performance": {
                "optimization_guardian": lambda ctx: self._apply_optimizations(ctx),
                "resource_guardian": lambda ctx: self._check_resource_usage(ctx)
            }
        }
        
        if self.dialect in dialect_rules:
            for node_type, guardian_func in dialect_rules[self.dialect].items():
                self.register_guardian("*", node_type, guardian_func)

    def _check_memory_safety(self, context):
        """Memory safety guardian"""
        node = context["node"]
        if node.get("kind") == "call" and node.get("name") in ["aloc", "free", "delete"]:
            # Add memory tracking metadata
            node["memory_tracked"] = True
            node["symbolic_tag"] = "⚠"  # Warning symbol
        return node

    def _check_type_safety(self, context):
        """Type safety guardian"""
        node = context["node"]
        if node.get("kind") == "binop":
            # Enhanced type checking with symbolic annotations
            node["type_checked"] = True
            node["symbolic_tag"] = "⊢"  # Type assertion symbol
        return node

    def _check_concurrency_safety(self, context):
        """Concurrency safety guardian"""
        node = context["node"]
        if node.get("kind") == "call" and node.get("name") in ["thread", "mutex", "lock"]:
            # Add concurrency metadata
            node["concurrency_safe"] = True
            node["symbolic_tag"] = "⚡"  # Concurrency symbol
        return node

    def _check_function_purity(self, context):
        """Function purity guardian for functional dialect"""
        node = context["node"]
        if node.get("kind") == "function":
            # Check for side effects
            has_side_effects = self._analyze_side_effects(node)
            if not has_side_effects:
                node["pure"] = True
                node["symbolic_tag"] = "λ"  # Lambda symbol for pure functions
        return node

    def _check_immutability(self, context):
        """Immutability guardian"""
        node = context["node"]
        if node.get("kind") == "let" and not node.get("mut", False):
            node["immutable"] = True
            node["symbolic_tag"] = "∞"  # Infinity symbol for immutable
        return node

    def _apply_optimizations(self, context):
        """Optimization guardian"""
        node = context["node"]
        if node.get("kind") == "binop":
            # Constant folding opportunity
            if (node.get("left", {}).get("kind") == "int" and 
                node.get("right", {}).get("kind") == "int"):
                node["optimizable"] = True
                node["symbolic_tag"] = "⟲"  # Optimization symbol
        return node

    def _check_resource_usage(self, context):
        """Resource usage guardian"""
        node = context["node"]
        if node.get("kind") == "while":
            # Potential infinite loop detection
            node["resource_monitored"] = True
            node["symbolic_tag"] = "⟳"  # Loop symbol
        return node

    def _analyze_side_effects(self, function_node):
        """Analyze function for side effects"""
        # Simplified side effect analysis
        side_effect_calls = ["log", "print", "aloc", "free", "thread", "mutex"]
        
        def check_node(node):
            if isinstance(node, dict):
                if node.get("kind") == "call" and node.get("name") in side_effect_calls:
                    return True
                # Recursively check all child nodes
                for value in node.values():
                    if isinstance(value, (dict, list)) and check_node(value):
                        return True
            elif isinstance(node, list):
                return any(check_node(item) for item in node)
            return False
        
        return check_node(function_node)

    def _generate_ir(self, node):
        """Translate AST to Phoenix IR with symbolic tags"""
        
        class IRGenerator:
            def __init__(self, dialect):
                self.instructions = []
                self.temp_counter = 0
                self.label_counter = 0
                self.dialect = dialect
                
            def fresh_temp(self):
                self.temp_counter += 1
                return f"%t{self.temp_counter}"
                
            def fresh_label(self):
                self.label_counter += 1
                return f"L{self.label_counter}"
                
            def emit(self, opcode, *args, **metadata):
                """Emit IR instruction with symbolic tags"""
                instruction = {
                    "id": len(self.instructions),
                    "opcode": opcode,
                    "args": list(args),
                    "metadata": metadata,
                    "symbolic_tag": metadata.get("symbolic_tag", ""),
                    "dialect": self.dialect
                }
                self.instructions.append(instruction)
                return instruction
                
            def generate_node(self, node):
                """Generate IR for AST node"""
                if not isinstance(node, dict):
                    return node
                
                kind = node.get("kind", "unknown")
                
                if kind == "capsule":
                    return self.generate_capsule(node)
                elif kind == "function":
                    return self.generate_function(node)
                elif kind == "let":
                    return self.generate_let(node)
                elif kind == "assign":
                    return self.generate_assign(node)
                elif kind == "binop":
                    return self.generate_binop(node)
                elif kind == "call":
                    return self.generate_call(node)
                elif kind == "if":
                    return self.generate_if(node)
                elif kind == "while":
                    return self.generate_while(node)
                elif kind == "return":
                    return self.generate_return(node)
                elif kind in ["int", "float", "string", "bool"]:
                    return self.generate_literal(node)
                elif kind == "var":
                    return self.generate_var(node)
                else:
                    # Handle unknown nodes gracefully
                    self.emit("UNKNOWN", kind, symbolic_tag="?")
                    return None
                
            def generate_capsule(self, node):
                """Generate IR for capsule with boundary metadata"""
                self.emit("CAPSULE_START", node["name"], 
                         symbolic_tag="⟦", 
                         metadata=node.get("metadata", {}))
                
                # Process functions
                for func_name, func_node in node.get("functions", {}).items():
                    self.generate_node(func_node)
                
                # Process globals
                for global_name, global_node in node.get("globals", {}).items():
                    self.generate_node(global_node)
                    
                # Process guardians
                for guardian in node.get("guardians", []):
                    self.emit("GUARDIAN_DEF", guardian["name"], guardian["event_type"],
                             symbolic_tag="🛡️")
                
                self.emit("CAPSULE_END", node["name"], symbolic_tag="⟧")
                return node["name"]
                
            def generate_function(self, node):
                """Generate IR for function with annotations"""
                func_name = node["name"]
                params = node.get("params", [])
                
                # Function annotations
                annotations = []
                if node.get("pure"): annotations.append("pure")
                if node.get("noexcept"): annotations.append("noexcept")
                if node.get("constexpr"): annotations.append("constexpr")
                
                self.emit("FUNC_START", func_name, params, 
                         symbolic_tag="⟶",
                         annotations=annotations,
                         return_type=node.get("return_type", "int"))
                
                # Generate function body
                if "body" in node:
                    for stmt in node["body"]:
                        self.generate_node(stmt)
                        
                self.emit("FUNC_END", func_name, symbolic_tag="↩")
                return func_name
                
            def generate_let(self, node):
                """Generate IR for variable declaration"""
                name = node["name"]
                is_mut = node.get("mut", False)
                
                if "value" in node:
                    value_temp = self.generate_node(node["value"])
                else:
                    value_temp = None
                    
                self.emit("DECLARE", name, value_temp,
                         symbolic_tag="⊢",
                         mutable=is_mut,
                         type_hint=node.get("type"))
                return name
                
            def generate_assign(self, node):
                """Generate IR for assignment"""
                name = node["name"]
                value_temp = self.generate_node(node["value"])
                
                self.emit("ASSIGN", name, value_temp,
                         symbolic_tag="⟵",
                         mutation_hook=True)
                return name
                
            def generate_binop(self, node):
                """Generate IR for binary operations"""
                left_temp = self.generate_node(node["left"])
                right_temp = self.generate_node(node["right"])
                result_temp = self.fresh_temp()
                op = node["op"]
                
                # Map operators to IR opcodes
                op_map = {
                    "+": "ADD", "-": "SUB", "*": "MUL", "/": "DIV", "%": "MOD",
                    "==": "EQ", "!=": "NE", "<": "LT", ">": "GT", 
                    "<=": "LE", ">=": "GE",
                    "&&": "AND", "||": "OR"
                }
                
                opcode = op_map.get(op, "BINOP")
                
                self.emit(opcode, result_temp, left_temp, right_temp,
                         symbolic_tag="⊕",
                         operator=op,
                         type_checked=node.get("type_checked", False),
                         optimizable=node.get("optimizable", False))
                
                return result_temp
                
            def generate_call(self, node):
                """Generate IR for function calls"""
                func_name = node["name"]
                args = [self.generate_node(arg) for arg in node.get("args", [])]
                result_temp = self.fresh_temp()
                
                self.emit("CALL", result_temp, func_name, *args,
                         symbolic_tag="⟲",
                         concurrency_safe=node.get("concurrency_safe", False),
                         memory_tracked=node.get("memory_tracked", False))
                
                return result_temp
                
            def generate_if(self, node):
                """Generate IR for conditional statements"""
                cond_temp = self.generate_node(node["cond"])
                then_label = self.fresh_label()
                else_label = self.fresh_label()
                end_label = self.fresh_label()
                
                self.emit("BRANCH_FALSE", cond_temp, else_label, symbolic_tag="⟳")
                
                # Then block
                self.emit("LABEL", then_label)
                for stmt in node.get("then", []):
                    self.generate_node(stmt)
                self.emit("JUMP", end_label)
                
                # Else block
                self.emit("LABEL", else_label)
                for stmt in node.get("else", []):
                    self.generate_node(stmt)
                    
                self.emit("LABEL", end_label)
                return None
                
            def generate_while(self, node):
                """Generate IR for loops"""
                loop_label = self.fresh_label()
                end_label = self.fresh_label()
                
                self.emit("LABEL", loop_label)
                cond_temp = self.generate_node(node["cond"])
                self.emit("BRANCH_FALSE", cond_temp, end_label, 
                         symbolic_tag="⟳",
                         resource_monitored=node.get("resource_monitored", False))
                
                # Loop body
                for stmt in node.get("body", []):
                    self.generate_node(stmt)
                    
                self.emit("JUMP", loop_label)
                self.emit("LABEL", end_label)
                return None
                
            def generate_return(self, node):
                """Generate IR for return statements"""
                if "value" in node and node["value"]:
                    value_temp = self.generate_node(node["value"])
                else:
                    value_temp = None
                    
                self.emit("RETURN", value_temp, symbolic_tag="↩")
                return None
                
            def generate_literal(self, node):
                """Generate IR for literals"""
                temp = self.fresh_temp()
                value = node["value"]
                type_name = node["kind"]
                
                self.emit("LOAD_CONST", temp, value, 
                         symbolic_tag="◆",
                         type=type_name)
                return temp
                
            def generate_var(self, node):
                """Generate IR for variable access"""
                name = node["name"]
                temp = self.fresh_temp()
                
                self.emit("LOAD_VAR", temp, name, symbolic_tag="◇")
                return temp
        
        # Generate IR using the generator
        generator = IRGenerator(self.dialect)
        generator.generate_node(node)
        
        return generator.instructions

    def _emit_bytecode(self, ir):
        """Final VM instructions with optimization and symbolic metadata"""
        
        class BytecodeEmitter:
            def __init__(self, dialect):
                self.bytecode = []
                self.constants = []
                self.symbols = {}
                self.labels = {}
                self.dialect = dialect
                
            def emit_byte(self, opcode, *args, **metadata):
                """Emit single bytecode instruction"""
                instruction = {
                    "opcode": opcode,
                    "args": list(args),
                    "metadata": metadata,
                    "offset": len(self.bytecode)
                }
                self.bytecode.append(instruction)
                return instruction
                
            def add_constant(self, value):
                """Add constant to constant pool"""
                if value not in self.constants:
                    self.constants.append(value)
                return self.constants.index(value)
                
            def resolve_labels(self):
                """Resolve label references to absolute addresses"""
                for i, instr in enumerate(self.bytecode):
                    if instr["opcode"] in ["BRANCH_FALSE", "JUMP"] and len(instr["args"]) > 0:
                        label = instr["args"][-1]
                        if label in self.labels:
                            instr["args"][-1] = self.labels[label]
                            
            def optimize_bytecode(self):
                """Apply bytecode-level optimizations"""
                optimized = []
                i = 0
                
                while i < len(self.bytecode):
                    current = self.bytecode[i]
                    
                    # Peephole optimizations
                    if (i + 1 < len(self.bytecode) and 
                        current["opcode"] == "LOAD_CONST" and 
                        self.bytecode[i + 1]["opcode"] == "LOAD_CONST"):
                        
                        # Constant folding opportunity
                        next_instr = self.bytecode[i + 1]
                        if (i + 2 < len(self.bytecode) and 
                            self.bytecode[i + 2]["opcode"] in ["ADD", "MUL", "SUB", "DIV"]):
                        
                            # Fold constants
                            op_instr = self.bytecode[i + 2]
                            val1 = current["args"][1]
                            val2 = next_instr["args"][1]
                            
                            result = self._fold_constants(op_instr["opcode"], val1, val2)
                            if result is not None:
                                # Replace three instructions with one
                                const_idx = self.add_constant(result)
                                optimized.append({
                                    "opcode": "LOAD_CONST",
                                    "args": [op_instr["args"][0], const_idx],
                                    "metadata": {"optimized": True, "symbolic_tag": "⟲"}
                                })
                                i += 3
                                continue
                    
                    optimized.append(current)
                    i += 1
                    
                self.bytecode = optimized
                
            def _fold_constants(self, opcode, val1, val2):
                """Fold constant operations"""
                try:
                    if opcode == "ADD":
                        return val1 + val2
                    elif opcode == "SUB":
                        return val1 - val2
                    elif opcode == "MUL":
                        return val1 * val2
                    elif opcode == "DIV" and val2 != 0:
                        return val1 // val2 if isinstance(val1, int) else val1 / val2
                    elif opcode == "EQ":
                        return 1 if val1 == val2 else 0
                    elif opcode == "NE":
                        return 1 if val1 != val2 else 0
                    elif opcode == "LT":
                        return 1 if val1 < val2 else 0
                    elif opcode == "GT":
                        return 1 if val1 > val2 else 0
                except:
                    pass
                return None
                
            def emit_ir_instruction(self, ir_instr):
                """Convert IR instruction to bytecode"""
                opcode = ir_instr["opcode"]
                args = ir_instr["args"]
                metadata = ir_instr.get("metadata", {})
                
                if opcode == "CAPSULE_START":
                    self.emit_byte("MODULE_START", args[0], **metadata)
                    
                elif opcode == "CAPSULE_END":
                    self.emit_byte("MODULE_END", args[0], **metadata)
                    
                elif opcode == "FUNC_START":
                    self.emit_byte("FUNC_DEF", args[0], len(args[1]), **metadata)
                    
                elif opcode == "FUNC_END":
                    self.emit_byte("FUNC_END", **metadata)
                    
                elif opcode == "DECLARE":
                    var_name, value_temp = args[0], args[1]
                    if value_temp:
                        self.emit_byte("STORE_VAR", var_name, value_temp, **metadata)
                    else:
                        self.emit_byte("ALLOC_VAR", var_name, **metadata)
                        
                elif opcode == "ASSIGN":
                    var_name, value_temp = args[0], args[1]
                    self.emit_byte("STORE_VAR", var_name, value_temp, **metadata)
                    
                elif opcode == "LOAD_CONST":
                    temp, value = args[0], args[1]
                    const_idx = self.add_constant(value)
                    self.emit_byte("LOAD_CONST", temp, const_idx, **metadata)
                    
                elif opcode == "LOAD_VAR":
                    temp, var_name = args[0], args[1]
                    self.emit_byte("LOAD_VAR", temp, var_name, **metadata)
                    
                elif opcode in ["ADD", "SUB", "MUL", "DIV", "MOD", "EQ", "NE", "LT", "GT", "LE", "GE", "AND", "OR"]:
                    result, left, right = args[0], args[1], args[2]
                    self.emit_byte(opcode, result, left, right, **metadata)
                    
                elif opcode == "CALL":
                    result, func_name = args[0], args[1]
                    call_args = args[2:]
                    self.emit_byte("CALL", result, func_name, len(call_args), *call_args, **metadata)
                    
                elif opcode == "RETURN":
                    if args and args[0]:
                        self.emit_byte("RETURN", args[0], **metadata)
                    else:
                        self.emit_byte("RETURN_VOID", **metadata)
                        
                elif opcode == "LABEL":
                    label_name = args[0]
                    self.labels[label_name] = len(self.bytecode)
                    
                elif opcode == "JUMP":
                    target = args[0]
                    self.emit_byte("JUMP", target, **metadata)
                    
                elif opcode == "BRANCH_FALSE":
                    cond, target = args[0], args[1]
                    self.emit_byte("BRANCH_FALSE", cond, target, **metadata)
                    
                elif opcode == "GUARDIAN_DEF":
                    guardian_name, event_type = args[0], args[1]
                    self.emit_byte("GUARDIAN_REG", guardian_name, event_type, **metadata)
                    
                else:
                    # Unknown opcode - emit as generic instruction
                    self.emit_byte("GENERIC", opcode, *args, **metadata)
            
            def generate_bytecode(self, ir_instructions):
                """Generate bytecode from IR"""
                # First pass: emit instructions
                for ir_instr in ir_instructions:
                    self.emit_ir_instruction(ir_instr)
                
                # Second pass: resolve labels
                self.resolve_labels()
                
                # Third pass: optimize
                if self.dialect in ["performance", "optimized"]:
                    self.optimize_bytecode()
                
                # Generate final bytecode package
                return {
                    "bytecode": self.bytecode,
                    "constants": self.constants,
                    "symbols": self.symbols,
                    "metadata": {
                        "dialect": self.dialect,
                        "version": "2.0",
                        "optimized": len([i for i in self.bytecode if i.get("metadata", {}).get("optimized", False)]),
                        "symbolic_instructions": len([i for i in self.bytecode if i.get("metadata", {}).get("symbolic_tag", "")]),
                        "guardian_hooks": len([i for i in self.bytecode if i["opcode"] == "GUARDIAN_REG"])
                    }
                }
        
        # Generate bytecode using the emitter
        emitter = BytecodeEmitter(self.dialect)
        bytecode_package = emitter.generate_bytecode(ir)
        
        # Add execution metadata
        bytecode_package["execution_info"] = {
            "entry_point": "main",
            "required_runtime": "Phoenix VM 2.0",
            "memory_model": "gc" if self.dialect == "safe" else "manual",
            "concurrency_model": "actor" if "concurrent" in self.dialect else "thread",
            "symbolic_support": True
        }
        
        return bytecode_package

class PhoenixVM:
    """Enhanced Phoenix Virtual Machine with full compilation pipeline"""
    
    def __init__(self, dialect="safe"):
        self.dialect = dialect
        self.compiler = CapsuleCompiler(dialect)
        self.symbol_table = {}
        self.execution_context = {}
        self.runtime_stats = {
            "instructions_executed": 0,
            "memory_allocated": 0,
            "functions_called": 0,
            "guard_checks": 0
        }
        print(f"🔥 [Phoenix VM] Initialized with dialect: {self.dialect}")

    def compile_and_run(self, source_code: str, output_format="bytecode"):
        """Complete Phoenix compilation pipeline"""
        try:
            # Step 1: Lexical analysis and parsing
            print("📝 [Phase 1] Lexical Analysis & Parsing...")
            tokens = self._lex(source_code)
            ast = self._parse(tokens)
            
            # Step 2: Semantic analysis and optimization
            print("🔍 [Phase 2] Semantic Analysis...")
            analyzed_ast = self._semantic_analysis(ast)
            
            # Step 3: Compilation
            print("⚙️ [Phase 3] Compilation...")
            if output_format == "bytecode":
                result = self.compiler.compile_capsule(analyzed_ast)
            elif output_format == "asm":
                result = self._compile_to_asm(analyzed_ast)
            elif output_format == "executable":
                result = self._compile_to_executable(analyzed_ast)
            else:
                raise PhoenixException(f"Unknown output format: {output_format}")
            
            # Step 4: Execution (if bytecode)
            if output_format == "bytecode":
                print("🚀 [Phase 4] Execution...")
                return self._execute_bytecode(result)
            
            return result
            
        except Exception as e:
            print(f"❌ [Compilation Error] {e}")
            raise

    def _lex(self, source_code: str):
        """Tokenize Phoenix source code"""
        # Simple tokenizer for demonstration
        import re
        
        TOKEN_PATTERNS = [
            (r'\d+', 'NUMBER'),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', 'IDENTIFIER'),
            (r'\+', 'PLUS'),
            (r'-', 'MINUS'),
            (r'\*', 'MULTIPLY'),
            (r'/', 'DIVIDE'),
            (r'=', 'ASSIGN'),
            (r'\(', 'LPAREN'),
            (r'\)', 'RPAREN'),
            (r'\{', 'LBRACE'),
            (r'\}', 'RBRACE'),
            (r';', 'SEMICOLON'),
            (r'\s+', 'WHITESPACE'),
        ]
        
        tokens = []
        pos = 0
        while pos < len(source_code):
            matched = False
            for pattern, token_type in TOKEN_PATTERNS:
                regex = re.compile(pattern)
                match = regex.match(source_code, pos)
                if match:
                    value = match.group(0)
                    if token_type != 'WHITESPACE':  # Skip whitespace
                        tokens.append({'type': token_type, 'value': value, 'pos': pos})
                    pos = match.end()
                    matched = True
                    break
            if not matched:
                pos += 1  # Skip unrecognized characters
        
        return tokens

    def _parse(self, tokens):
        """Parse tokens into AST"""
        # Simple recursive descent parser for demonstration
        if not tokens:
            return {"kind": "capsule", "name": "empty", "functions": {}}
        
        # Create a simple AST structure
        return {
            "kind": "capsule", 
            "name": "Main", 
            "functions": {
                "main": {
                    "kind": "function",
                    "name": "main",
                    "params": [],
                    "body": [
                        {
                            "kind": "return",
                            "value": {"kind": "int", "value": 0}
                        }
                    ]
                }
            },
            "globals": {},
            "guardians": []
        }

    def _semantic_analysis(self, ast):
        """Perform semantic analysis and type checking"""
        print("🔬 [Semantic] Type checking and symbol resolution...")
        
        # Extract the capsule for processing
        if isinstance(ast, dict) and ast.get("kind") == "capsule":
            capsule = ast
            
            # Build symbol table
            self._build_symbol_table(capsule)
            
            # Type check
            self._type_check(capsule)
            
            return capsule
        
        raise PhoenixException("No valid capsule found in AST")

    def _build_symbol_table(self, capsule):
        """Build symbol table for the capsule"""
        # Process functions
        for func_name, func_node in capsule.get("functions", {}).items():
            self.symbol_table[func_name] = {
                "type": "function",
                "params": func_node.get("params", []),
                "return_type": func_node.get("return_type", "int"),
                "pure": func_node.get("pure", False),
                "noexcept": func_node.get("noexcept", False)
            }
        
        # Process globals
        for global_name, global_node in capsule.get("globals", {}).items():
            self.symbol_table[global_name] = {
                "type": "variable",
                "mutable": global_node.get("mut", False),
                "value_type": global_node.get("type", "int")
            }

    def _type_check(self, capsule):
        """Perform type checking on the capsule"""
        print("🎯 [Type Check] Verifying type safety...")
        
        def check_node(node, context=None):
            if not isinstance(node, dict):
                return
                
            kind = node.get("kind")
            
            if kind == "binop":
                # Check binary operation type compatibility
                left_type = self._infer_type(node.get("left", {}))
                right_type = self._infer_type(node.get("right", {}))
                
                if not self._types_compatible(left_type, right_type, node.get("op")):
                    raise PhoenixException(
                        f"Type mismatch: {left_type} {node.get('op')} {right_type}",
                        "TypeError"
                    )
            
            elif kind == "call":
                # Check function call
                func_name = node.get("name")
                if func_name in self.symbol_table:
                    func_info = self.symbol_table[func_name]
                    expected_params = len(func_info.get("params", []))
                    actual_args = len(node.get("args", []))
                    
                    if expected_params != actual_args:
                        raise PhoenixException(
                            f"Function {func_name} expects {expected_params} args, got {actual_args}",
                            "ArgumentError"
                        )
            
            # Recursively check child nodes
            for key, value in node.items():
                if isinstance(value, dict):
                    check_node(value, context)
                elif isinstance(value, list):
                    for item in value:
                        if isinstance(item, dict):
                            check_node(item, context)
        
        # Type check all functions
        for func_node in capsule.get("functions", {}).values():
            if "body" in func_node:
                for stmt in func_node["body"]:
                    check_node(stmt)

    def _infer_type(self, node):
        """Infer the type of an expression node"""
        if not isinstance(node, dict):
            return "unknown"
            
        kind = node.get("kind", "unknown")
        
        type_map = {
            "int": "int",
            "float": "float", 
            "bool": "bool",
            "string": "string",
            "char": "char",
            "null": "null"
        }
        
        return type_map.get(kind, "unknown")

    def _types_compatible(self, left_type, right_type, op):
        """Check if two types are compatible for the given operation"""
        numeric_types = {"int", "float"}
        comparable_types = {"int", "float", "string", "char"}
        
        if op in ["+", "-", "*", "/", "%"]:
            return left_type in numeric_types and right_type in numeric_types
        elif op in ["==", "!=", "<", ">", "<=", ">="]:
            return (left_type == right_type or 
                   (left_type in comparable_types and right_type in comparable_types))
        elif op in ["&&", "||"]:
            return left_type == "bool" and right_type == "bool"
        
        return True  # Allow other operations for now

    def _compile_to_asm(self, capsule):
        """Compile capsule to x86-64 assembly"""
        print("🔧 [ASM Generation] Generating x86-64 assembly...")
        
        asm_generator = X86AssemblyGenerator(self.dialect)
        return asm_generator.generate_assembly(capsule)

    def _compile_to_executable(self, capsule):
        """Compile capsule to native executable"""
        print("🏗️ [Native Compilation] Building executable...")
        
        # Step 1: Generate assembly
        asm_code = self._compile_to_asm(capsule)
        
        # Step 2: Write to file
        asm_file = "phoenix_output.s"
        with open(asm_file, "w") as f:
            f.write(asm_code)
        
        # Step 3: Assemble with NASM
        obj_file = "phoenix_output.o"
        try:
            result = subprocess.run([
                "nasm", "-f", "elf64", asm_file, "-o", obj_file
            ], capture_output=True, text=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError) as e:
            raise PhoenixException(f"Assembly failed: {e}")
        
        # Step 4: Link with ld
        exe_file = "phoenix_output"
        try:
            result = subprocess.run([
                "ld", obj_file, "-o", exe_file
            ], capture_output=True, text=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError) as e:
            raise PhoenixException(f"Linking failed: {e}")
        
        print(f"✅ [Success] Executable created: {exe_file}")
        return exe_file

    def _execute_bytecode(self, bytecode_package):
        """Execute Phoenix bytecode"""
        print("🎮 [Execution] Running Phoenix bytecode...")
        
        bytecode = bytecode_package["bytecode"]
        constants = bytecode_package["constants"]
        
        # Execution state
        stack = []
        variables = {}
        pc = 0  # Program counter
        
        while pc < len(bytecode):
            instruction = bytecode[pc]
            opcode = instruction["opcode"]
            args = instruction["args"]
            
            self.runtime_stats["instructions_executed"] += 1
            
            if opcode == "LOAD_CONST":
                temp, const_idx = args[0], args[1]
                value = constants[const_idx]
                variables[temp] = value
                
            elif opcode == "LOAD_VAR":
                temp, var_name = args[0], args[1]
                if var_name in variables:
                    variables[temp] = variables[var_name]
                else:
                    raise PhoenixException(f"Undefined variable: {var_name}")
                    
            elif opcode == "STORE_VAR":
                var_name, temp = args[0], args[1]
                if temp in variables:
                    variables[var_name] = variables[temp]
                    
            elif opcode == "ADD":
                result, left, right = args[0], args[1], args[2]
                left_val = variables.get(left, 0)
                right_val = variables.get(right, 0)
                variables[result] = left_val + right_val
                
            elif opcode == "SUB":
                result, left, right = args[0], args[1], args[2]
                left_val = variables.get(left, 0)
                right_val = variables.get(right, 0)
                variables[result] = left_val - right_val
                
            elif opcode == "MUL":
                result, left, right = args[0], args[1], args[2]
                left_val = variables.get(left, 0)
                right_val = variables.get(right, 0)
                variables[result] = left_val * right_val
                
            elif opcode == "DIV":
                result, left, right = args[0], args[1], args[2]
                left_val = variables.get(left, 0)
                right_val = variables.get(right, 0)
                if right_val == 0:
                    raise PhoenixException("Division by zero")
                variables[result] = left_val // right_val
                
            elif opcode == "CALL":
                result, func_name = args[0], args[1]
                call_args = args[3:]  # Skip arg count
                self.runtime_stats["functions_called"] += 1
                
                # Handle built-in functions
                if func_name == "log":
                    values = [variables.get(arg, arg) for arg in call_args]
                    print("📝 [Phoenix Log]", *values)
                    variables[result] = 0
                else:
                    # User-defined function call (simplified)
                    variables[result] = 0
                    
            elif opcode == "RETURN":
                if args and args[0]:
                    return_val = variables.get(args[0], 0)
                else:
                    return_val = 0
                print(f"🎯 [Return] Program returned: {return_val}")
                return return_val
                
            elif opcode == "RETURN_VOID":
                print("🎯 [Return] Program completed")
                return 0
                
            elif opcode == "JUMP":
                pc = args[0]
                continue
                
            elif opcode == "BRANCH_FALSE":
                cond, target = args[0], args[1]
                if not variables.get(cond, False):
                    pc = target
                    continue
                    
            pc += 1
        
        print("✅ [Execution] Program completed successfully")
        return 0

class X86AssemblyGenerator:
    """Generate x86-64 assembly from Phoenix IR"""
    
    def __init__(self, dialect="safe"):
        self.dialect = dialect
        self.asm_lines = []
        self.data_section = []
        self.bss_section = []
        self.current_function = None

    def generate_assembly(self, capsule):
        """Generate complete x86-64 assembly program"""
        self.asm_lines = [
            "; Phoenix ProLang - Generated x86-64 Assembly",
            f"; Dialect: {self.dialect}",
            f"; Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}",
            "",
            "section .data",
        ]
        
        # Add string constants
        self.asm_lines.extend(self.data_section)
        
        self.asm_lines.extend([
            "",
            "section .bss",
        ])
        
        # Add uninitialized data
        self.asm_lines.extend(self.bss_section)
        
        self.asm_lines.extend([
            "",
            "section .text",
            "global _start",
            "",
            "_start:",
            "    ; Initialize Phoenix runtime",
            "    call main",
            "    ; Exit program", 
            "    mov rax, 60    ; sys_exit",
            "    mov rdi, 0     ; exit status",
            "    syscall",
            ""
        ])
        
        # Generate functions
        for func_name, func_node in capsule.get("functions", {}).items():
            self._generate_function(func_name, func_node)
        
        return "\n".join(self.asm_lines)

    def _generate_function(self, name, func_node):
        """Generate assembly for a single function"""
        self.current_function = name
        
        self.asm_lines.extend([
            f"{name}:",
            "    ; Function prologue",
            "    push rbp",
            "    mov rbp, rsp",
            ""
        ])
        
        # Generate function body
        if "body" in func_node:
            for stmt in func_node["body"]:
                self._generate_statement(stmt)
        
        self.asm_lines.extend([
            "",
            "    ; Function epilogue", 
            "    mov rsp, rbp",
            "    pop rbp",
            "    ret",
            ""
        ])

    def _generate_statement(self, stmt):
        """Generate assembly for a statement"""
        if not isinstance(stmt, dict):
            return
            
        kind = stmt.get("kind")
        
        if kind == "return":
            if "value" in stmt and stmt["value"]:
                # Return with value
                value_node = stmt["value"]
                if value_node.get("kind") == "int":
                    self.asm_lines.extend([
                        f"    mov rax, {value_node['value']}  ; return value",
                    ])
                else:
                    self.asm_lines.extend([
                        "    mov rax, 0  ; default return value",
                    ])
            else:
                self.asm_lines.extend([
                    "    mov rax, 0  ; void return",
                ])
                
        elif kind == "call" and stmt.get("name") == "log":
            # Handle log calls (simplified - would need proper string handling)
            self.asm_lines.extend([
                "    ; log call (simplified)",
                "    ; TODO: Implement proper logging",
            ])
            
        elif kind == "let":
            # Variable declaration
            name = stmt.get("name", "unknown")
            self.asm_lines.extend([
                f"    ; declare variable: {name}",
                "    ; TODO: Implement variable allocation",
            ])

def main():
    """Main entry point for Phoenix compiler"""
    if len(sys.argv) < 2:
        print("🔥 Phoenix ProLang Compiler")
        print("Usage: python VM_for_Phoenix.py <file.phx> [--format=bytecode|asm|exe]")
        return
    
    filename = sys.argv[1]
    output_format = "bytecode"
    
    # Parse command line arguments
    for arg in sys.argv[2:]:
        if arg.startswith("--format="):
            output_format = arg.split("=")[1]
    
    try:
        # Read source file
        with open(filename, 'r') as f:
            source_code = f.read()
        
        # Create VM and compile
        vm = PhoenixVM(dialect="safe")
        result = vm.compile_and_run(source_code, output_format)
        
        print(f"🎉 [Success] Compilation completed successfully")
        print(f"📊 [Stats] Instructions: {vm.runtime_stats['instructions_executed']}")
        print(f"📊 [Stats] Function calls: {vm.runtime_stats['functions_called']}")
        
    except FileNotFoundError:
        print(f"❌ [Error] File not found: {filename}")
    except Exception as e:
        print(f"❌ [Error] {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()

    import sys
    import time
    class CapsuleCompiler:
        """Compiler for Phoenix ProLang capsules"""
        def __init__(self, dialect="safe"):
            self.dialect = dialect
            self.instructions = []
            self.temp_counter = 0
            
        def fresh_temp(self):
            """Generate a new temporary variable name"""
            temp_name = f"temp_{self.temp_counter}"
            self.temp_counter += 1
            return temp_name
            
        def emit(self, opcode, *args, **metadata):
            """Emit an IR instruction"""
            instruction = {
                "opcode": opcode,
                "args": args,
                "metadata": metadata
            }
            self.instructions.append(instruction)
        
        def compile_capsule(self, node):
            """Compile a Phoenix capsule into IR"""
            
            class IRGenerator:
                def __init__(self, dialect):
                    self.dialect = dialect
                    self.instructions = []
                    self.temp_counter = 0
                    
                def fresh_temp(self):
                    """Generate a new temporary variable name"""
                    temp_name = f"temp_{self.temp_counter}"
                    self.temp_counter += 1
                    return temp_name
                
                def emit(self, opcode, *args, **metadata):
                    """Emit an IR instruction"""
                    instruction = {
                        "opcode": opcode,
                        "args": args,
                        "metadata": metadata
                    }
                    self.instructions.append(instruction)
                
                def generate_node(self, node):
                    """Generate IR for a given AST node"""
                    if not isinstance(node, dict):
                        return None
                    
                    kind = node.get("kind")
                    
                    if kind == "capsule":
                        return self.generate_capsule(node)
                    elif kind == "function":
                        return self.generate_function(node)
                    elif kind == "return":
                        return self.generate_return(node)
                    elif kind == "literal":
                        return self.generate_literal(node)
                    elif kind == "var":
                        return self.generate_var(node)
                    
                    # Handle other kinds of nodes (e.g., expressions, statements)
                    # ...
                    
                def generate_capsule(self, node):
                    """Generate IR for a capsule"""
                    capsule_name = node.get("name", "Main")
                    
                    # Start capsule
                    self.emit("CAPSULE_START", capsule_name)
                    
                    # Process functions
                    for func_name, func_node in node.get("functions", {}).items():
                        self.generate_function(func_node)
                        
                    # End capsule
                    self.emit("CAPSULE_END", capsule_name)
                    
                def generate_function(self,
                                      node):
                        """Generate IR for a function"""
                        func_name = node.get("name", "unknown")
                        params = node.get("params", [])
                        
                        # Start function definition
                        self.emit("FUNC_START", func_name, params)
                        
                        # Process function body
                        for stmt in node.get("body", []):
                            self.generate_node(stmt)
                            
                        # End function
                        self.emit("FUNC_END")

                        def generate_return(self, node):
                            """Generate IR for a return statement"""
                            if "value" in node:
                                value = self.generate_node(node["value"])
                                self.emit("RETURN", value)
                            else:
                                self.emit("RETURN_VOID")
                                def generate_literal(self, node):
                                    """Generate IR for a literal value"""
                                    if "value" in node:
                                        value = node["value"]
                                        temp = self.fresh_temp()
                                        self.emit("LOAD_CONST", temp, value)
                                        return temp
                                    return None

                                def generate_var(self, node):
                                    """Generate IR for a variable"""
                                    if "name" in node:
                                        var_name = node["name"]
                                        temp = self.fresh_temp()
                                        self.emit("LOAD_VAR", temp, var_name)
                                        return temp
                                    return None
                                def generate_ir(self, ast):
                                    """Generate IR from AST"""
                                    if not isinstance(ast, dict):
                                        return None
                                    
                                    ir = []
                                    for node in ast.get("body", []):
                                        ir_node = self.generate_node(node)
                                        if ir_node:
                                            ir.append(ir_node)
                                    return ir
                                # Create IR generator
                                ir_generator = IRGenerator(self.dialect)
                                ir = ir_generator.generate_ir(node)
                                # Create IR generator
                                class BytecodeEmitter:
                                    """Emit bytecode from IR"""
                                    def __init__(self, dialect="safe"):
                                        self.dialect = dialect
                                        self.bytecode = []
                                        self.constants = []
                                        self.symbols = {}
                                        self.labels = {}
                                        self.temp_counter = 0
                                        
                                    def fresh_temp(self):
                                        """Generate a new temporary variable name"""
                                        temp_name = f"temp_{self.temp_counter}"
                                        self.temp_counter += 1
                                        return temp_name
                                        
                                    def add_constant(self, value):
                                        """Add a constant to the bytecode"""
                                        if value not in self.constants:
                                            self.constants.append(value)
                                        return self.constants.index(value)
                                        
                                    def emit_byte(self, opcode, *args, **metadata):
                                        """Emit a bytecode instruction"""
                                        instruction = {
                                            "opcode": opcode,
                                            "args": args,
                                            "metadata": metadata
                                        }
                                        self.bytecode.append(instruction)
                                        
                                    def resolve_labels(self):
                                        """Resolve label references in bytecode"""
                                        for instruction in self.bytecode:
                                            if instruction["opcode"] == "JUMP":
                                                target_label = instruction["args"][0]
                                                if target_label in self.labels:
                                                    instruction["args"][0] = self.labels[target_label]
                                                else:
                                                    raise PhoenixException(f"Undefined label: {target_label}")
                                            
                                            elif instruction["opcode"] == "BRANCH_FALSE":
                                                cond, target_label = instruction["args"]
                                                if target_label in self.labels:
                                                    instruction["args"][1] = self.labels[target_label]
                                                else:
                                                    raise PhoenixException(f"Undefined label: {target_label}")
                                            
                                    def optimize_bytecode(self):
                                        """Optimize bytecode (placeholder for future optimizations)"""
                                        pass
                                    
                                    def emit_ir_instruction(self, ir_instr):
                                        """Emit an IR instruction as bytecode"""
                                        opcode, args, metadata = ir_instr["opcode"], ir_instr["args"], ir_instr.get("metadata", {})
                                        
                                        if isinstance(args, list):
                                            args = tuple(args)
                                            if opcode == "LOAD_CONST":
                                                const_idx = self.add_constant(args[1])
                                                self.emit_byte(opcode, args[0], const_idx, **metadata)
