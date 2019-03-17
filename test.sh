#!/bin/bash

./AcDc '../test/sample'${1}'.ac' 'output'${1}
dc 'output'${1}
