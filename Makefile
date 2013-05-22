all:
	gcc -shared -fPIC -o libmemutil.so libmemutil.c 
	gcc -shared -fPIC -ldb-4.8 -o libberkeley-db.so libberkeley-db.c 
