#!/bin/bash
#  Description : Testing suite for ECDSA implementation in Haskell.
#  Author:     : Bc. Vít Barták (xbarta47)
#  Maintainer  : xbarta47@fit.vutbr.cz
#  Year        : 2023
# For more info, read the comments in test.sh


set -e
echo "[INFO] Testing the ECDSA-Haskell program with $(ls curves/ | tr '\n' ' ') curves."

echo -e "\n------ [TESTING] secp128r1 ------"
sleep 1
./test.sh curves/secp128r1 128

echo -e "\n------ [TESTING] secp192r1 ------"
sleep 1
./test.sh curves/secp192r1 128

echo -e "\n------ [TESTING] secp256k1 ------"
sleep 1
./test.sh curves/secp256k1 256

echo -e "\n------ [TESTING] secp384r1 ------"
sleep 1
./test.sh curves/secp384r1 384

echo -e "\n------ [TESTING] ansip192k1 ------"
sleep 1
./test.sh curves/ansip192k1 256

echo -e "\n------ [TESTING] brainpoolP384r1 ------"
sleep 1
./test.sh curves/brainpoolP384r1 384

echo -e "\n------ [TESTING] FRP256v1 ------"
sleep 1
./test.sh curves/FRP256v1 256

echo -e "\n------ [TESTING] P224 ------"
sleep 1
./test.sh curves/P224 256

echo -e "\n------ [TESTING] Pallas ------"
sleep 1
./test.sh curves/Pallas 384

echo -e "\n\e[1;32m[SUCCESS] All tests successful.\e[0m"
