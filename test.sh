#!/bin/bash

./AcDc '../nkhg/hard'${1}'.ac' 'output'${1}
dc 'output'${1}
