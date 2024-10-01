cmake_minimum_required(VERSION 3.28)

project(lox)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_VERBOSE_MAKEFILE True)
# TODO: Add -Werror later
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 -Wall -Wextra")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS False)
remove_definitions("-DNDEBUG")

add_executable(lox main.cpp error.cpp token.cpp ast.cpp visitor.cc)
