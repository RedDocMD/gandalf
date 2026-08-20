import pyverilog.vparser.ast as AST
from pyverilog.vparser.parser import parse

filename = 'netlist/puzzle_fixed.v'
ast, directives = parse([filename])

desc = ast.children()[0]
module_def = desc.children()[0]

clk_wires = {}
clk_outs = []
floating_clk = []

for child in module_def.children():
    if not isinstance(child, AST.InstanceList):
        continue
    instance = child.instances[0]
    is_clkbuf = 'clkbuf' in instance.module
    for port in instance.portlist:
        if is_clkbuf and port.portname == 'X':
            if port.argname:
                clk_outs.append(port.argname.name)
        elif not is_clkbuf and port.portname == 'CLK':
            if port.argname:
                outs = clk_wires.get(port.argname.name, [])
                outs.append(instance.name)
                clk_wires[port.argname.name] = outs
            else:
                floating_clk.append(instance.name)

print(','.join(floating_clk))
for clk_wire in clk_wires:
    if clk_wire not in clk_outs and clk_wire != 'clk':
        print(f'{clk_wire} is not driven but drives {",".join(clk_wires[clk_wire])}')