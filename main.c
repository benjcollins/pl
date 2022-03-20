#include <stdio.h>

extern int test();

int main() {
    for (int i = 0; i < 20; i++) {
        printf("%d\n", test(i));
    }
}