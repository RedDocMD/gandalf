import pyverilog.vparser.ast as AST
from pyverilog.vparser.parser import parse

filename = 'netlist/puzzle_fixed.v'
ast, directives = parse([filename])

desc = ast.children()[0]
module_def = desc.children()[0]

nodes = {}
wire_inp = {}
wire_out = {}
clk_next = []

for child in module_def.children():
    if not isinstance(child, AST.InstanceList):
        continue
    instance = child.instances[0]
    # if 'clkbuf' not in instance.module:
    #     continue
    nodes[instance.name] = instance
    for port in instance.portlist:
        if port.argname is None:
            continue
        if isinstance(port.argname, AST.Pointer):
            arg = f'{port.argname.var}[{port.argname.ptr}]'
        else:
            arg = port.argname.name
        if (port.portname == 'A' and 'clkbuf' in instance.module) or port.portname == 'CLK': # Input
            outs = wire_out.get(arg, [])
            outs.append(instance.name)
            wire_out[arg] = outs
            if arg == 'clk':
                clk_next.append(instance.name)
        elif port.portname == 'X' and 'clkbuf' in instance.module: # Output
            part = wire_inp.get(arg, None)
            if part:
                raise RuntimeError(f"{arg} already has an input {part}, trying to add another: {instance.name}")
            wire_inp[arg] = instance.name

adj_list = {}
in_adj_list = set()
for node_name, node in nodes.items():
    if 'clkbuf' not in node.module:
        continue
    for port in node.portlist:
        if port.portname != 'X' or port.argname is None:
            continue
        arg = port.argname.name
        adj_list[node_name] = wire_out.get(arg, [arg])
        in_adj_list.update(adj_list[node_name])

with open("/tmp/monk.dot", "w") as f:
    print("digraph clkbuf {", file=f)
    for name in clk_next:
        print(f"clk -> {name}", file=f)
    for name, nexts in adj_list.items():
        for next_name in nexts:
            print(f"{name} -> {next_name}", file=f)
    for wire, outs in wire_out.items():
        for out in outs:
            if not out in in_adj_list:
                print(f'{wire} -> {out}')
    print("}", file=f)





































































