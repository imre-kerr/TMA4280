project(ex4)
enable_language(Fortran)
cmake_minimum_required(VERSION 2.8)

find_package(OpenMP REQUIRED)
find_package(MPI REQUIRED)

list(APPEND CMAKE_Fortran_FLAGS ${MPI_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS} -ffree-form)

include_directories(${MPI_Fortran_INCLUDE_PATH})


add_executable(single single.f)

add_executable(openmp openmp.f)

add_executable(mpi mpi.f)
target_link_libraries(mpi ${MPI_Fortran_LIBRARIES})

add_executable(both both.f)
target_link_libraries(both ${MPI_Fortran_LIBRARIES})