#!/bin/bash

# Make wrapper script, can be invoked from any directory in the sources
# Find the root directory by going to .. until .project-root exists 
# determine the build type, must be first target name 
# if build/<type> does not exist, invoke cmake to create the build configuration
# Run make -C build/<type> <rest of args> for each mentioned bulid type

# EXAMPLES
#
#  make-wrapper.sh
#     Builds build/debug, make cmdline is make -C build/debug
#  make-wrapper release
#     Builds build/release, make cmdline is make -C build/release
#  make-wrapper debug release
#     Builds both by running make twice in build/debug and build/release
#  make-wrapper test
#     Builds tests in build/debug, make cmdline is make -C build/debug test
#  make-wrapper -j6 debug release clean all test
#     Make cmdline #1 is make -C build/debug -j6 clean all test
#     Make cmdline #2 is make -C build/release -j6 clean all test

oldpwd="$PWD"
while [ $PWD != / -a ! -f .project-root ] ; do
    cd ..
done
if [ ! -f .project-root ] ; then
    cd "$oldpwd"
    echo "Unable to find .project-root, running /bin/make from $PWD"
    exec /bin/make $*
fi

#echo "In the project root dir: $PWD"

# dictionary of possible build types, key is string like debug/release and value is CMAKE name for it
declare -A possible_buildtypes
# list of build types that we are going to build
declare -a buildtypes_to_build
declare -a args

default_build_type=debug
possible_buildtypes=([debug]=Debug [release]=Release)
buildtypes_to_build=()
args=()

while [ x${1:0:1} == x- ] ; do
    args=(${args[*]} $1)
    shift
done

# any initial args can be a build type indicating that we want to build that one
while [ x"$1" != x ] ; do
    if [ x${possible_buildtypes[$1]} != x ] ; then
        buildtypes_to_build=(${buildtypes_to_build[*]} $1)
        shift
    else
        break
    fi
done

if [ "x${buildtypes_to_build[*]}" == x ] ; then
    buildtypes_to_build=($default_build_type)
fi

#echo "Args are ${args[@]}"
#echo "Buildtypes to build are: ${buildtypes_to_build[@]}"

# Possibly call cmake to create build dirs
for btype in ${buildtypes_to_build[@]} ; do
    if [ ! -d build/$btype ] ; then
        cmake -S . -B build/$btype -DHUNTER_NO_TOOLCHAIN_ID_RECALCULATION=ON -DCMAKE_BUILD_TYPE=${possible_buildtypes[$btype]} || exit $?
    fi
done

# build it
for btype in ${buildtypes_to_build[@]} ; do
    make -C build/$btype ${args[@]} $@ || exit $?
done


#make -C build/$buildtype
