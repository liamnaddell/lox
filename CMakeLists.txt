cmake_minimum_required(VERSION 3.28)

project(lox)

set(CMAKE_CXX_STANDARD 20)

add_executable(lox)
target_sources(lox
    PUBLIC
    main.cpp
)
target_sources(lox
  PUBLIC
    FILE_SET all_my_modules TYPE CXX_MODULES FILES
    Hello.cppm
)
