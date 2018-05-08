//
//  main.cpp
//  subtype
//
//  Created by User on 5/7/18.
//  Copyright © 2018 Xu Sun. All rights reserved.
//

#include <iostream>
#include <set>


#include <string>
using namespace std;


template <typename T>

bool isSubset(const T&v1,const T&v2) {
    for(auto const& v1v:v1){
        if(find(v2.begin(),v2.end(),v1v)==v2.end())
            return false;
    }
    return true;
}



template <typename T>

void if_is_subset(const T&v1,const T&v2,const T&v3){
    
    if(!isSubset(v1, v2)){
        cout << "Stuck" <<endl;
    }else{
        cout << "The first set is a subset of the second set" << endl;
        if(!isSubset(v2, v3)){
            cout << "Stuck" <<endl;
        }else{
            cout << "The Second set is a subset of the Third set" << endl;
            if(isSubset(v1, v3)){
                cout << "Subtype Correct" <<endl;
            }        }
    }
    
}

template <typename T,typename S>

bool is_element_Subset(const T&v1,const S&v2) {
    
        if(find(v2.begin(),v2.end(),v1)==v2.end())
            return false;
    
    return true;
}

template <typename T,typename S>

void if_element_is_subset(const T&v1,const S&v2,const S&v3){
    
    if(!is_element_Subset(v1, v2)){
        cout << "Stuck" <<endl;
    }else{
        cout << "The first set is a subset of the second set" << endl;
        if(!isSubset(v2, v3)){
            cout << "Stuck" <<endl;
        }else{
            cout << "The Second set is a subset of the Third set" << endl;
            if(is_element_Subset(v1, v3)){
                cout << "Subtype Correct" <<endl;
            }        }
    }
    
}


int main()
{
    
    set<string> a;
    a.insert("1");
    a.insert("2");
    a.insert("3");
    
    set<string> b;
    b.insert("1");
    b.insert("2");
    b.insert("3");
    b.insert("4");
    
    set<string> c;
    c.insert("1");
    c.insert("2");
    c.insert("3");
    c.insert("4");
    c.insert("5");
    
    cout << "Test 1:" <<endl;
    if_is_subset(a, b, c);
    
    a.insert("7");
    cout << "Test 2:" <<endl;
    if_is_subset(a, b, c);
    
    b.insert("7");
    b.insert("9");
    cout << "Test 3:" <<endl;
    if_is_subset(a, b, c);
    
    string d = "1";
    string e = "a";
    
    cout << "Test 4:" <<endl;
    if_element_is_subset(d, b, c);
    
    cout << "Test 5:" <<endl;
    if_element_is_subset(e, b, c);
    
    c.insert("7");
    c.insert("9");
    
    cout << "Test 6:" <<endl;
    if_element_is_subset(d, b, c);
    
    
    
    

    return 0;
}








