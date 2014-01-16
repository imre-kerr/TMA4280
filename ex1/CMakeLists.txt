project(super_ex1)
enable_language(Fortran)
cmake_minimum_required(VERSION 2.6)

list(APPEND CMAKE_Fortran_FLAGS "-ffree-form")
add_executable(first first.f)
add_executable(quad quad.f)