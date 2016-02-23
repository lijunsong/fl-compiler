#!/bin/bash

ocamldebug `ocamlfind query -recursive -i-format batteries sexplib oUnit` -I _build ./main.byte $@
