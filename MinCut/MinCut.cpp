#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>
#include <set>
#include <map>
#include <random>
#include <future>
#include <thread>
#include <atomic>

using Group = std::set<int>;
using AdjList = std::map<Group, std::set<Group>>;

std::ostream &operator<<(std::ostream &out, const std::set<int> &s) {
    out << '(';
    for (auto e : s) out << e << ',';
    out << ')';
    return out;
}

std::ostream &operator<<(std::ostream &out, const std::set<Group> &s) {
    for (auto e : s) out << e << ' ';
    return out;
}

std::ostream &operator<<(std::ostream &out, const AdjList &g) {
    for (auto it : g) out << it.first << ": " << it.second << std::endl;
    return out;
}

AdjList readInput(const std::string &fileName) {
    std::ifstream in(fileName);
    AdjList r;
    int curr;
    while (in >> curr) {
        std::string line;
        std::getline(in, line);
        std::stringstream ss(line);
        std::set<Group> vertices;
        int v;
        while (ss >> v) {
            vertices.insert(Group{ v });
        }
        r[Group{ curr }] = vertices;
        // std::cout << Group{ curr } << ": " << vertices << std::endl;
    }
    return r;
}

static struct {
    std::mt19937 gen = std::mt19937{ 41 };
    std::mutex mutex;

    template <typename Container>
    int operator()(const Container &cont) {
        std::unique_lock<std::mutex> lock(mutex);
        return std::uniform_int_distribution<>(0, cont.size() - 1)(gen);
    }
} randInt;

AdjList iterativeMerge(AdjList g) {
    while (g.size() > 2) {
        auto selected = std::next(g.begin(), randInt(g));

        Group m1k = selected->first;
        auto m1v = g[m1k];

        Group m2k = *std::next(selected->second.begin(), randInt(selected->second));
        auto m2v = g[m2k];

        // remove self links
        m1v.erase(m2k);
        m2v.erase(m1k);

        Group mergedK = m1k;
        mergedK.insert(m2k.begin(), m2k.end());

        auto mergedV = m1v;
        mergedV.insert(m2v.begin(), m2v.end());

        // remove 2 nodes from graph
        g.erase(m1k);
        g.erase(m2k);

        // insert into graph
        g[mergedK] = mergedV;

        // update other nodes
        for (auto p : mergedV) {
            if (g[p].find(m1k) != g[p].end()) {
                g[p].erase(m1k);
                g[p].insert(mergedK);
            }
            if (g[p].find(m2k) != g[p].end()) {
                g[p].erase(m2k);
                g[p].insert(mergedK);
            }
        }

        // std::cout << "Iteration size " << g.size() << std::endl;
        // std::cout << g << std::endl;
    }
    return g;
}

int minCut(AdjList g, AdjList ref) {
    int crosses = 0;
    auto l = g.begin()->first;
    auto r = std::next(g.begin())->first;
    for (auto a : l) {
        for (auto b : r) {
            // count just once
            if (ref[Group{ a }].find(Group{ b }) != ref[Group{ a }].end()) {
                ++crosses;
            }
        }
    }
    return crosses;
}

int main(int argc, char *argv[]) {
    if (argc < 2) return -1;
    const auto input = readInput(argv[1]);
    int minCutSoFar;
    std::mutex mutex;

    minCutSoFar = std::numeric_limits<int>::max();
    const int nThreads = std::thread::hardware_concurrency();
    std::vector<std::thread> threads;
    for (int t = 0; t < nThreads; ++t) {
        threads.emplace_back([=, &minCutSoFar, &mutex] {
            for (int i = 0; i < input.size() * input.size(); ++i) {
                int cuts = minCut(iterativeMerge(input), input);
                {
                    std::unique_lock<std::mutex> lock(mutex);
                    if (cuts < minCutSoFar) std::cout << minCutSoFar << " > " << cuts << std::endl;
                    minCutSoFar = std::min(minCutSoFar, cuts);
                }
            }
        });
    }
    for (auto &t : threads)
        if (t.joinable()) t.join();
    std::cout << "minCut " << minCutSoFar << std::endl;
}
