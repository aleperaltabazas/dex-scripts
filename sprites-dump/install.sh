#!/bin/bash

DIR=$PWD

sudo cp dex-sprites-dump "/usr/local/bin/dex-sprites-dump"
sudo chmod +x "/usr/local/bin/dex-sprites-dump"
sudo sed -i --expression "s@<dex-sprites-dump-root>@$DIR@g" /usr/local/bin/dex-sprites-dump
