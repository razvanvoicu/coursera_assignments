#include <iostream>
#include <algorithm>
#include <string>
#include <list>
#include <climits>
#include <stdio.h>

using namespace std;

int main() {
	string line;
	getline(cin,line);
	int s = 0;
	list<int> * result = new list<int>();
	list<int> skews;
	int min = INT_MAX;
	for ( int i = 0 ; i < line.length() ; i ++) {
		if ( line[i] == 'C' ) s-- ;
		else if ( line[i] == 'G' ) s++ ;
		if ( s < min ) {
			min = s;
			delete result;
			result = new list<int>({i+1});
		} else if ( s == min ) result->push_back(i+1) ;
		skews.push_back(s);
	}
	for ( auto i = result->begin() ; i != result->end() ; i++ ) cout << " " << *i ;
	cout << endl ;
	//for ( int i = 0 ; i < line.length() ; i ++ ) cout << "  " << line[i] ;
	//cout << endl ;
	//for ( auto i = skews.begin() ; i != skews.end() ; i++ ) printf("%3d",*i);
	//cout << endl ;
}