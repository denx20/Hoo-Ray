# Fetches a graph from the output of `run-graph.hs`
import networkx as nx
import matplotlib.pyplot as plt
from pathlib import Path
from ast import literal_eval
from networkx.drawing.nx_agraph import graphviz_layout

beginning_string = '<-- Starting Dependency Graph Generation -->'
ending_string = '<-- End of Dependency Graph Generation -->'

graph_file = Path(__file__).parent / '..' / 'graph.out'
graph_file.resolve()

G = nx.DiGraph()

with open(graph_file) as f:
    lines = f.readlines()
    lines = [l.strip() for l in lines]

    graph_lines = lines[lines.index(beginning_string) + 1: lines.index(ending_string)]
    parsed_objects = list(map(literal_eval, graph_lines))

    for parent, children in parsed_objects:
        G.add_node(parent)
        G.add_nodes_from(children)
        for child in children:
            G.add_edge(child, parent)

options = {
    # 'font_color': 'white',
    'node_color': 'yellow',
    'node_size': 1000,
    'edgecolors': 'black',
    'width': 1,
    'arrowstyle': '-|>',
    'arrowsize': 12,
}

# nx.nx_agraph.write_dot(G,'temp.dot')
pos = graphviz_layout(G, prog='dot')
subax1 = plt.subplot(111)
nx.draw(G, pos, with_labels=True, arrows=True, **options)

plt.savefig('graph.png')