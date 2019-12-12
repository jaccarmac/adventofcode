guix environment --ad-hoc sbcl cl-slime-swank -- sbcl --eval "(require 'asdf)" --eval "(require 'swank)" --eval "(swank:create-server)"
