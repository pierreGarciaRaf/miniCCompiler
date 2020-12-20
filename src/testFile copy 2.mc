void a(){
    2;
}
int b(int z, int t){
    a();
    3;
}
void c(){
    b(2,3);
    b(2+3,0);
    return b(a(),2);
}