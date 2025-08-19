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
    class PhoenixException(Exception):
        def __init__(self, message, error_type):
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
        class PhoenixVM:
            def __init__(self, dialect="safe"):
                self.dialect = dialect
                self.symbol_table = {}
                self.guardians = {}
                self.execution_context = {}
                print(f"🛠️ [VM Initialized] Dialect: {self.dialect}")
                def compile(self, node):
                    """Compile AST node to Phoenix bytecode with symbolic metadata"""
                    # Step 1: Guard the AST node
                    guarded_node = self._guard_ast(node)
                    
                    # Step 2: Generate IR from the guarded AST
                    ir = self._generate_ir(guarded_node)
                    
                    # Step 3: Emit bytecode from the IR
                    bytecode = self._emit_bytecode(ir)
                    
                    return bytecode
                def _guard_ast(self, node):
                    """Guard AST nodes with symbolic metadata and dialect-specific rules"""
                    
                    def traverse_and_guard(ast_node, current_path="root"):
                        """Recursively traverse AST and apply guardians"""
                        if not isinstance(ast_node, dict):
                            return ast_node
                        # Check for guardians on the current node
                        for node_type, guardians in self.guardians.items():
                            if node_type == "*" or ast_node.get("kind") == node_type:
                                for guardian_name, guardian_func in guardians.items():
                                    try:
                                        # Create context for the guardian
                                        guard_context = {
                                            "node": ast_node,
                                            "path": current_path,
                                            "dialect": self.dialect,
                                            "symbol_table": self.symbol_table,
                                            "metadata": {
                                                "symbolic_tags": {
                                                    "⟶": "function_arrow", 
                                                    "⟦": "capsule_start",
                                                    "⟧": "capsule_end",
                                                    "↩": "return"
                                                },
                                                "dialect": self.dialect,
                                                "symbols": self.symbol_table
                                            },
                                            "symbolic_tags": {
                                                "⟦": "capsule_start",
                                                "⟧": "capsule_end",
                                                "⟶": "function_arrow",
                                                "↩": "return"
                                            }
                                        }
                                        # Apply the guardian function
                                        guarded_result = guardian_func(guard_context)
                                        if guarded_result is not None:
                                            ast_node = guarded_result
                                        print(f"🔒 [Guardian Applied] {guardian_name} on {current_path}")
                                    except Exception as e:
                                        raise PhoenixException(f"Guardian {guardian_name} failed: {str(e)}", "GuardianError")
                        
                        # Recursively traverse children
                        for key, value in ast_node.items():
                            if isinstance(value, dict):
                                ast_node[key] = traverse_and_guard(value, f"{current_path}.{key}")
                            elif isinstance(value, list):
                                ast_node[key] = [traverse_and_guard(item, f"{current_path}.{key}[{i}]") 
                                               for i, item in enumerate(value)]
                        
                        return ast_node
                    
                    return traverse_and_guard(node)

                    # Define dialect-specific rules for guardians
                    dialect_rules = {
                        "safe": {
                            "memory_safety_guardian": lambda ctx: self._check_memory_safety(ctx),
                            "type_safety_guardian": lambda ctx: self._check_type_safety(ctx),
                            "concurrency_safety_guardian": lambda ctx: self._check_concurrency_safety(ctx)
                            },
                        "functional": {
                            "function_purity_guardian": lambda ctx: self._check_function_purity(ctx),
                            "immutability_guardian": lambda ctx: self._check_immutability(ctx)
                        },
                        "performance": {
                            "optimization_guardian": lambda ctx: self._apply_optimizations(ctx),
                            "resource_usage_guardian": lambda ctx: self._check_resource_usage(ctx)
                        }
                        }
                    # Register guardians based on dialect
                    for rule_name, rule_func in dialect_rules.get(self.dialect, {}).items():
                        self.register_guardian("*", rule_name, rule_func)
                        def _check_memory_safety(self, context):
                            """Memory safety guardian"""
                            node = context["node"]
                            if node.get("kind") == "alloc" or node.get("kind") == "free":
                                # Add memory safety metadata
                                node["memory_safe"] = True
                                node["symbolic_tag"] = "🧠"
                                return node
                            return node
                        def _check_type_safety(self, context):
                            """Type safety guardian"""
                            node = context["node"]
                            if node.get("kind") == "let" and not node.get("type_checked", False):
                                # Add type safety metadata
                                node["type_safe"] = True
                                node["symbolic_tag"] = "🔍"
                            return node
                        def _check_concurrency_safety(self, context):
                            """Concurrency safety guardian"""
                            node = context["node"]
                            if node.get("kind") == "thread" or node.get("kind") == "mutex":
                                # Add concurrency safety metadata
                                node["concurrency_safe"] = True
                                node["symbolic_tag"] = "🔗"
                            return node

