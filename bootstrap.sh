#!/usr/bin/env bash

emacs --batch --load org readme.org -f org-babel-tangle
emacs --batch --load init.el

# Python tooling
CONFIG="${HOME}/.config"
LOCAL_BIN="${HOME}/.local/bin"

create_symbolic_links () {
    if [ -d "${LOCAL_BIN}" ]; then
        ln -fs ${PYLS}/bin/pyls ${LOCAL_BIN}
        ln -fs ${PYLS}/bin/flake8 ${LOCAL_BIN}
    else
        echo "Directory ${LOCAL_BIN} does not exist"
        echo "Please make sure ${LOCAL_BIN} exists and that is's available on your \$PATH"
    fi
}

setup_pyls () {
    PYLS="$HOME/.pyls"

    python3 -m venv ${PYLS}
    ${PYLS}/bin/pip install --upgrade \
           pip \
           setuptools \
           wheel
    ${PYLS}/bin/pip install --upgrade \
           python-language-server \
           flake8 \
           flake8-builtins \
           flake8-commas \
           flake8-docstrings \
           flake8-import-order \
           flake8-logging-format

    create_symbolic_links
}

read -r -p "Do you want to install 'python-language-server' with 'flake8'? [y/N] " response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
    setup_pyls
fi

write_flake8_config () {
    if [ -d "${CONFIG}" ]; then
        cat > ${CONFIG}/flake8 <<EOF
[flake8]
import-order-style = smarkets
enable-extensions = G  # Validate logging format strings
ignore =
    # Ignore missing docstrings
    D100
    D101
    D102
    D103
    D104
    D105
    D106
    D107
EOF
    else
        echo "Directory ${CONFIG} does not exist"
        echo "Please make sure ${CONFIG} exists"
    fi
}

read -r -p "Do you want to create a default flake8 config file in '${CONFIG}'? [y/N] " response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
    write_flake8_config
fi
