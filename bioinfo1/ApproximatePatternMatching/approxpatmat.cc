#include <iostream>
#include <algorithm>
#include <string>
#include <list>

using namespace std;

int hamming(string s1, int i1, string s2, int length) {
	int count = 0 ;
	for ( auto i = 0 ; i < length ; i ++ )
		if (s1[i1+i] != s2[i]) count ++ ;
	return count ;
}

int main() {
	string pattern, line;
	int dist;
	getline(cin,pattern);
	getline(cin,line);
	cin >> dist;
    list<int> result;
	for ( int i = 0; i < line.length() - pattern.length() + 1; i ++ )
		if ( hamming(line,i,pattern,pattern.length()) <= dist) result.push_back(i) ;
	for ( auto i = result.begin() ; i != result.end() ; i++ ) cout << *i << " " ;
	cout << endl ;
}