#include <cstdio>

extern "C" {
int printi(int val) {
    return printf("%d\n", val);
}
}
