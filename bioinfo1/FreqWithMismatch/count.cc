#include <algorithm>
#include <iostream>
#include <string>
#include <list>

using namespace std;	

int hamming(string& s1, int i1, string& s2, int length) {
	int count = 0 ;
	for ( auto i = 0 ; i < length ; i ++ )
		if (s1[i1+i] != s2[i]) count ++ ;
	return count ;
}

int count(string& line, string& pattern, int dist) {
	int c = 0;
	for ( int i = 0; i < line.length() - pattern.length() + 1; i ++ )
		if ( hamming(line,i,pattern,pattern.length()) <= dist) {
			c ++ ;
		}
	return c;
}

int main() {
	string line("CATGCCATTCGCATTGTCCCAGTGA");
	string pat("CCC");
	cout << count(line,pat,2) << endl ;
}