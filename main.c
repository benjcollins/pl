#include <stdio.h>

void print_int(int n) {
    printf("%d\n", n);
}

struct Point {
    int x;
    int y;
};

void print_point(struct Point *p) {
    printf("(%d, %d)\n", p->x, p->y);
}