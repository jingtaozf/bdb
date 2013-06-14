all:
	#gcc -shared -fPIC -o libmemutil.so libmemutil.c 
	gcc -shared -fPIC -ldb-4.8 -o libbdb.so libbdb.c 
