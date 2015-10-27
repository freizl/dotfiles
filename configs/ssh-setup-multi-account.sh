#!/bin/sh

ssh-add -D
ssh-add id_rsa
ssh-add id_rsa_freizl
ssh-add -l
ssh -T personal
ssh -T work
