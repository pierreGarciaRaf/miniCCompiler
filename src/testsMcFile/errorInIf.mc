int PARAM = 5;

void truc(){2;}

int fact(int n) {
    if (truc()) {
        return 1;
    } else {
        return n * fact(n + -1);
    }
}

void main() {
    putchar(fact(PARAM));
}
