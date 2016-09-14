#!/bin/bash

# sphinx-apidoc -o ./source/ ../python_driver/
make html
chromium-browser build/html/index.html
