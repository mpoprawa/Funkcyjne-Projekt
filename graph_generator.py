import random

def gen_k_components(n, k):
    nodes = list(range(n))
    random.shuffle(nodes)

    sizes = [n // k] * k
    for i in range(n % k):
        sizes[i] += 1

    index = 0
    edges = []

    for size in sizes:
        component_nodes = nodes[index:index+size]
        index += size

        for i in range(1, size):
            u = component_nodes[i]
            v = random.choice(component_nodes[:i])
            edges.append((u, v))

    adj = {}
    for u, v in edges:
        adj.setdefault(u, []).append(v)
        adj.setdefault(v, []).append(u)
    return adj

def save_to_file(adj_list, filename):
    with open(filename, 'w') as f:
        for node in sorted(adj_list):
            neighbors = ','.join(str(nei) for nei in sorted(adj_list[node]))
            f.write(f"{node}: {neighbors}\n")

graph = gen_k_components(20, 3)
save_to_file(graph, "graphs/components.txt")