#!/bin/bash

# Copyright (C) 2015  Nicolas Lamirault <nicolas.lamirault@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

set -e
if [ -z "$1" ]; then
    echo -e "\033[31;01m[dionysos] Pass the version number as the first arg. E.g.: $0 1.2.3 \033[0m"
    exit 1
fi
if [ -z "$GITHUB_TOKEN" ]; then
    echo -e "\033[31;01m[dionysos] GITHUB_TOKEN must be set for github-release \033[0m"
    exit 1
fi

VERSION=$1

git tag $VERSION
git push --tags

echo -e "\033[32;01m[dionysos] Build image \033[0m"
docker build -t dionysos/release addons

echo -e "\033[32;01m[dionysos] Make release \033[0m"
docker run --rm -e GITHUB_TOKEN dionysos/release \
    github-release release \
    --user nlamirault \
    --repo dionysos \
    --tag $VERSION \
    --name $VERSION \
    --description ""
    # --pre-release \

echo -e "\033[32;01m[dionysos] Upload archive \033[0m"
PKG="dionysos-$VERSION.tar.gz"
docker run --rm -e GITHUB_TOKEN -v `pwd`:/src/dionysos \
       dionysos/release github-release upload \
       --user nlamirault \
       --repo dionysos \
       --tag $VERSION \
       --name $PKG \
       --file /src/dionysos/$PKG
