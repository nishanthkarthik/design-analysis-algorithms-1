#include <iostream>
#include <vector>
#include <fstream>
#include <map>
#include <unordered_map>
#include <sstream>
#include <algorithm>

using Graph = std::map<int, std::vector<int>>;

std::ostream &operator<<(std::ostream &out, const std::map<int, int>& m) {
    for (auto e : m) out << '{' << e.first << ',' << e.second << "} ";
    return out;
}

std::ostream &operator<<(std::ostream &out, const std::vector<int> &v) {
    for (auto e : v) out << e << ' ';
    return out;
}

std::ostream &operator<<(std::ostream &out, const Graph &g) {
    for (auto t : g) out << t.first << ": " << t.second << std::endl;
    return out;
}

Graph readInput(std::string fileName) {
    std::ifstream in{ fileName };
    int src;
    Graph res;
    while (in >> src) {
        std::string line;
        std::getline(in, line);
        std::stringstream stream(line);
        int dst;
        while (stream >> dst) res[src].push_back(dst);
    }
    return res;
}

Graph reverseGraph(const Graph &in) {
    Graph out;
    for (const auto &it : in) {
        for (auto e : it.second)
            out[e].push_back(it.first);
    }
    return out;
}

constexpr auto magic = 900000;

std::vector<int> finishingTime(const Graph &g) {
    std::vector<int> ft(magic, -1);
    std::vector<bool> visited(magic, false);
    int timer = 0;
    std::vector<int> stack;
    stack.reserve(magic);

    auto dfs = [&] {
        while (!stack.empty()) {
            int cur = stack.back();
            stack.pop_back();

            if (cur < 0) {
                ft[timer++] = -cur;
                continue;
            }

            if (visited[cur]) continue;
            stack.push_back(-cur);

            visited[cur] = true;

            const auto &edges = g.find(cur);
            if (edges == g.end()) continue;
            for (auto it = edges->second.rbegin(); it != edges->second.rend(); ++it)
                if (!visited[*it]) stack.push_back(*it);
        }
    };

    for (auto it = g.rbegin(); it != g.rend(); ++it) {
        int node = it->first;
        if (visited[node]) continue;
        stack.push_back(node);
        dfs();
    }

    ft.resize(timer);
    return ft;
}

std::map<int, int> connectedComponents(const Graph &g, const std::vector<int> &ft) {
    std::vector<bool> visited(magic, false);
    std::map<int, int> componentCount;
    std::vector<int> stack;
    stack.reserve(magic);

    int leader = -1;

    auto dfs = [&] {
        while (!stack.empty()) {
            int cur = stack.back();
            stack.pop_back();

            if (visited[cur]) continue;

            ++componentCount[leader];
            visited[cur] = true;

            const auto &edges = g.find(cur);
            if (edges == g.end()) continue;
            for (auto it = edges->second.rbegin(); it != edges->second.rend(); ++it)
                if (!visited[*it]) stack.push_back(*it);
        }
    };

    for (auto it = ft.rbegin(); it != ft.rend(); ++it) {
        if (visited[*it]) continue;
        leader = *it;
        stack.push_back(*it);
        dfs();
    }
    return componentCount;
}

int main(int argc, char *argv[]) {
    if (argc < 2) return 1;
    const auto g = readInput(argv[1]);
    std::cout << g.size() << std::endl;

    const auto gr = reverseGraph(g);
    std::cout << gr.size() << std::endl;

    const auto ft = finishingTime(gr);
    std::cout << ft.size() << std::endl;

    const auto cc = connectedComponents(g, ft);
    std::cout << cc.size() << std::endl;

    std::vector<int> weights;
    for (auto it = cc.begin(); it != cc.end(); ++it)
        weights.push_back(it->second);
    std::sort(weights.rbegin(), weights.rend());

    std::cout << "weights ";
    for (int i = 0; i < std::min(weights.size(), 5UL); ++i)
        std::cout << weights[i] << ' ';
    std::cout << std::endl;
}
