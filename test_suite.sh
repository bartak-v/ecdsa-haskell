#!/bin/bash
# For more info, read the comments in test.sh
set -e
echo "[INFO] Testing the ECDSA-Haskell program with secp128r1, secp256k1 and secp384r1 curves."

echo "[TESTING] secp128r1"
./test.sh curves/secp128r1 128 1>/dev/null
echo "[TESTING] secp256k1"
./test.sh curves/secp256k1 256 &>/dev/null
echo "[TESTING] secp384r1"
./test.sh curves/secp384r1 384 &>/dev/null

echo "[SUCCESS] All tests successful."