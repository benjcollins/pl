#include <stdio.h>

extern void my_main();

void print_int(int n) {
    printf("%d\n", n);
}

int main() {
    my_main();
}