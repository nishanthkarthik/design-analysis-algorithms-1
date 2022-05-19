load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

haskell_binary(
    name = "num-inversions-hs",
    srcs = ["NumInversions/NumInversions.hs"],
    deps = [
        "@stackage//:HUnit",
        "@stackage//:base",
        "@stackage//:mtl",
        "@stackage//:vector",
    ],
    data = ["NumInversions/tests.txt", "NumInversions/IntegerArraySpaced.txt"],
)

load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "num-inversions-cpp",
    srcs = ["NumInversions/NumInversions.cpp"],
    data = ["NumInversions/tests.txt", "NumInversions/IntegerArraySpaced.txt"],
)
