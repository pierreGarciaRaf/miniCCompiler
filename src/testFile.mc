int a(){
    2;
}
int b(int z){
    a();
    3;
}
void c(){
    return b(a());
}