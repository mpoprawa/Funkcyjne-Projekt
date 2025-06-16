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

        extra_edges_count = max(1, (size*size) // 30)
        existing_edges = set((min(u,v), max(u,v)) for u,v in edges)
        added = 0

        while added < extra_edges_count:
            u, v = random.sample(component_nodes, 2)
            edge = (min(u,v), max(u,v))
            if edge not in existing_edges:
                edges.append((u, v))
                existing_edges.add(edge)
                added += 1

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

graph = gen_k_components(100, 3)
save_to_file(graph, "graphs/small.txt")