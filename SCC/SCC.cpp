#include <iostream>
#include <vector>
#include <fstream>
#include <map>
#include <sstream>

using Graph = std::map<int, std::vector<int>>;

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

int main(int argc, char *argv[]) {
    if (argc < 2) return 1;
    auto g = readInput(argv[1]);
    std::cout << g << std::endl;
    std::cout << reverseGraph(g) << std::endl;
}
