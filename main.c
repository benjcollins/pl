#include <stdio.h>

extern void incr(int *n);

int main() {
    int n = 2;
    incr(&n);
    printf("%d\n", n);
}