#!/usr/bin/env bash

emacs --batch --load org readme.org -f org-babel-tangle
emacs --batch --load init.el

# Python tooling
CONFIG="${HOME}/.config"
LOCAL_BIN="${HOME}/.local/bin"
PYLSP="$HOME/.pylsp"

create_symbolic_links () {
    if [ -d "${LOCAL_BIN}" ]; then
        ln -fs ${PYLSP}/bin/pylsp ${LOCAL_BIN}
        ln -fs ${PYLSP}/bin/flake8 ${LOCAL_BIN}
    else
        echo "Directory ${LOCAL_BIN} does not exist"
        echo "Please make sure ${LOCAL_BIN} exists and that is's available on your \$PATH"
    fi
}

setup_pyls () {
    python3 -m venv ${PYLSP}
    ${PYLSP}/bin/pip install --upgrade \
           pip \
           setuptools \
           wheel
    ${PYLSP}/bin/pip install --upgrade \
           python-lsp-server \
           flake8 \
           flake8-builtins \
           flake8-commas \
           flake8-docstrings \
           flake8-import-order \
           flake8-logging-format \
           pyls-isort \
           yapf

    create_symbolic_links
}

read -r -p "Do you want to install 'python-lsp-server'? [y/N] " response
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
