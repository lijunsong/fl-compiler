#!/bin/bash

ocamldebug `ocamlfind query -recursive -i-format batteries sexplib` -I _build ./main.byte $@
