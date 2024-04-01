#!/bin/bash

#########################################
##  
##  SOURCE: Inspired by Yago Iglesias Vazquez
##  DESCRIPTION: Written in a C project
##
#########################################

has_failed=false

TESTS_FOLDER="file_tests"
INPUT_EXT="input"
EXP_OUTPUT_EXT="expected_output"
OUTPUT_EXT="output"
TEMP_DIFF_FILE=".diff_temp"
EXEC="dune exec prjt_mi_recolor"

############################
# Color print functions #
############################

yellow() {
    echo -e "\e[33m$@\e[0m"
}

green() {
    echo -e "\e[32m$@\e[0m"
}

red() {
    echo -e "\e[31m$@\e[0m"
}

blue() {
    echo -e "\e[1m\e[34m$@\e[0m"
}

############################
# Init and cleaning functions #
############################

function init_dirs(){
    if [ ! -e "$TESTS_FOLDER" ];
    then
        mkdir "$TESTS_FOLDER"
    fi
    if [ ! -e "$TESTS_FOLDER/$OUTPUT_EXT" ];
    then
        mkdir "$TESTS_FOLDER/$OUTPUT_EXT"
    fi
    if [ ! -e "$TESTS_FOLDER/$INPUT_EXT" ];
    then
        mkdir "$TESTS_FOLDER/$INPUT_EXT"
    fi
    if [ ! -e "$TESTS_FOLDER/$EXP_OUTPUT_EXT" ];
    then
        mkdir "$TESTS_FOLDER/$EXP_OUTPUT_EXT"
    fi
    return 0
}

function clean_temp_files(){
    rm -f "$TEMP_DIFF_FILE"
}

function clean_output_dir(){
    [ -d "$TESTS_FOLDER/$OUTPUT_EXT" ] && rm -rf "$TESTS_FOLDER/$OUTPUT_EXT"
    mkdir "$TESTS_FOLDER/$OUTPUT_EXT"
}

###################
# Tests functions #
###################

function test_one_output(){
    local file=$1
    local expected_output_file="$TESTS_FOLDER/$EXP_OUTPUT_EXT/$file"
    local output_file="$TESTS_FOLDER/$OUTPUT_EXT/$file"

    if [ ! -e "$output_file" ] || [ ! -f "$output_file" ];
    then
        red "FAIL: The output file $file does not exist or has not the good format"
        has_failed=true
        return 1
    fi

    if  ! diff -y  $expected_output_file $output_file > $TEMP_DIFF_FILE; # Compare expected output and output
    then
        red "FAIL: The output and expected output $file does not match..."
        has_failed=true
        cat $TEMP_DIFF_FILE
        echo "---------"
        echo
        return 1
    fi
    green "PASSED: The output and expected output $output_file match !"
    return 0
}

function test_outputs(){
    echo
    echo "-- Test outputs --"
    echo
    local has_passed=0
    local expected_output_files;
    expected_output_files=$(find "$TESTS_FOLDER/$EXP_OUTPUT_EXT" -type f -name "*.txt" -printf "%f\n")
    
    for file in $expected_output_files; do
        test_one_output $file || has_passed=1
    done

    if [ $has_passed -eq 1 ];
    then
        has_failed=true
    fi
    
    return $has_passed
}

###########################
# Build and exec function #
###########################

function build_exec_output(){
    echo
    echo "-- Build outputs --"
    echo
    local input_files;
    input_files=$(find "$TESTS_FOLDER/$INPUT_EXT" -type f -name "*.txt" -printf "%f\n")
    local has_passed=0

    for file in $input_files; do
        $EXEC "$TESTS_FOLDER/$INPUT_EXT/$file" "$TESTS_FOLDER/$OUTPUT_EXT/$file" 2> /dev/null || has_passed=1 
 
        if [ $has_passed -eq 1 ];
        then
            red "FAIL: The input $file could not be executed..."
            has_passed=0
            has_failed=true
        else
            green "PASSED: The input $file has been executed !"
        fi
    done


    return $has_passed
}

#################
# Main function #
#################

function main(){
    init_dirs
    clean_output_dir
    build_exec_output
    test_outputs
    clean_output_dir
    clean_temp_files
}

main

if $has_failed; then
    exit 1
fi
exit 0
