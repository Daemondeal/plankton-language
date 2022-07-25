[ ! -d "./plankton_target" ] && mkdir plankton_target

cargo debug

cd plankton_target

gcc output.c && echo "Program compiled. Running..." && ./a.out 

rm ./a.out

cd ..
