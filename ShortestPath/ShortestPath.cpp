#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <sstream>
#include <queue>
#include <limits>

template <typename T>
std::ostream &operator<<(std::ostream &out, const std::vector<T> &vec) {
    out << '[';
    for (auto e : vec) out << e << ',';
    out << ']';
    return out;
}

template <typename T>
std::ostream &operator<<(std::ostream &out, const std::map<int, T> &m) {
    out << '{';
    for (auto e : m) out << e.first << ':' << e.second << ',';
    out << '}';
    return out;
}

using Graph = std::map<int, std::map<int, int>>;

Graph readInput(const std::string &fileName) {
    Graph g;
    std::ifstream in{ fileName };
    int from;
    while (in >> from) {
        int to, len;
        char comma;
        std::string line;
        std::getline(in, line);
        std::stringstream str(line);
        while (str >> to >> comma >> len) {
            g[from][to] = len;
        }
    }
    return g;
}

struct ComparePairsSndDec {
    bool operator()(std::pair<int, int> a, std::pair<int, int> b) { return a.second > b.second; };
};

std::vector<long> shortestPath(const Graph &g, int start) {
    const auto maximum = 1 + g.rbegin()->first;
    std::vector<bool> visited(maximum, false);
    std::vector<long> dist(maximum, std::numeric_limits<long>::max());
    std::priority_queue<std::pair<int, int>, std::vector<std::pair<int, int>>, ComparePairsSndDec> q;

    dist[start] = 0;
    q.push({start, 0});

    while (!q.empty()) {
        auto top = q.top();
        q.pop();
        auto node = top.first;
        if (visited[node]) continue;

        for (auto it : g.find(node)->second) {
            auto edge = it.first;
            auto length = it.second;
            dist[edge] = std::min(dist[edge], dist[node] + length);
            q.push({edge, dist[edge]});
        }

        visited[node] = true;
    }

    return dist;
}

int main(int argc, char *argv[]) {
    if (argc < 2) return -1;
    const auto graph = readInput(argv[1]);
    std::cout << graph.size() << std::endl;

    const auto lengths = shortestPath(graph, 1);

    for (auto node : { 7, 37, 59, 82, 99, 115, 133, 165, 188, 197 }) {
        if (node < lengths.size()) {
            std::cout << node << ": " << lengths[node] << std::endl;
        }
    }
    std::cout << std::endl;
}
