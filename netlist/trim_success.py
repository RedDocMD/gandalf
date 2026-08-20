import pyverilog.vparser.ast as AST
from pyverilog.vparser.parser import VerilogCodeParser, parse
from pyverilog.ast_code_generator.codegen import ASTCodeGenerator
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import tempfile


class Interface:
    def __init__(self, name, inputs, outputs):
        self.name = name
        self.inputs = inputs
        self.outputs = outputs


def get_file_module_interface(filename):
    with tempfile.TemporaryDirectory() as temp_dir:
        parser = VerilogCodeParser([filename], debug=False, 
                                  outputdir=temp_dir,
                                  preprocess_output=f'{temp_dir}/preprocess.output')
        ast = parser.parse()
    desc = ast.children()[0]
    module_def = desc.children()[0]
    portlist = module_def.portlist
    inputs = []
    outputs = []
    for port in portlist.ports:
        if not isinstance(port, AST.Ioport):
            continue
        pin = port.first
        if isinstance(pin, AST.Input):
            inputs.append(pin.name)
        elif isinstance(pin, AST.Output):
            outputs.append(pin.name)
        else:
            raise RuntimeError(f"Unknown direction {type(pin)}")
    return Interface(module_def.name, inputs, outputs)


interfaces = {}
modules_path = Path('cells/verilog')
with ThreadPoolExecutor(max_workers=8) as executor:
    for intf in executor.map(get_file_module_interface, [str(c) for c in modules_path.iterdir()]):
        interfaces[intf.name] = intf


filename = 'netlist/puzzle_fixed.v'
ast, _ = parse([filename])
desc = ast.children()[0]
module_def = desc.children()[0]

nodes = set()
edges = {}

for child in module_def.children():
    if not isinstance(child, AST.InstanceList):
        continue
    instance = child.instances[0]
    intf = interfaces[instance.module]
    nodes.add(instance.name)
    for port in instance.portlist:
        if port.argname is None:
            continue
        if isinstance(port.argname, AST.Pointer):
            arg = f'{port.argname.var}[{port.argname.ptr}]'
        else:
            arg = port.argname.name
        nodes.add(arg)
        # Reverse the edges here
        if port.portname in intf.outputs:
            adj = edges.get(arg, [])
            adj.append(instance.name)
            edges[arg] = adj
        else:
            adj = edges.get(instance.name, [])
            adj.append(arg)
            edges[instance.name] = adj

stack = ['success']
reachable = set()
while len(stack) > 0:
    top = stack.pop()
    if top in reachable:
        continue
    reachable.add(top) 
    for adj in edges.get(top, []):
        stack.append(adj)

print(f"{len(reachable)} nodes out of {len(module_def.items)}")

new_items = []
for child in module_def.items:
    if not isinstance(child, AST.InstanceList):
        new_items.append(child)
        continue
    instance = child.instances[0]
    if instance.name in reachable:
        new_items.append(child)
module_def.items = tuple(new_items)

codegen = ASTCodeGenerator()
rslt = codegen.visit(ast)
outfile = 'netlist/puzzle_success.v'
with open(outfile, "w") as f:
    f.write(rslt)