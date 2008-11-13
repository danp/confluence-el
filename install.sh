EMACS_LIB_DIR=$HOME/.emacs-lib
CONFLUENCE_DIR=$HOME/.confluence

echo "About to install confluence.el into your emacs library directory."
echo ""
read -p "Emacs library directory ($EMACS_LIB_DIR): " ANSWER
if [ ! -z "$ANSWER" ]; then
  EMACS_LIB_DIR=$ANSWER
fi


echo "installing confluence.el into $EMACS_LIB_DIR"
test -f $EMACS_LIB_DIR/confluence.el || cp confluence.el $EMACS_LIB_DIR

echo "installing confluence.scm into $CONFLUENCE_DIR"
test -d $CONFLUENCE_DIR || mkdir -p $CONFLUENCE_DIR
test -f $CONFLUENCE_DIR/confluence.scm || cp confluence.scm $CONFLUENCE_DIR/

read -p "The URL to your confluence instance: " CNF_URL
read -p "You confluence Username: " CNF_USR

echo
echo "Please append the following to your .emacs:"
echo 

echo "  (load \"confluence.el\")"
echo "  (setq *confluence-xml-rpc-url* \"${CNF_URL}/rpc/xmlrpc\")"
echo "  (cnf-library-init \"${CNF_USR}\") "
echo 
echo "The *confluence-xml-rpc-url* should point to your instance's "
echo "xml-rpc url, it should be something like:"
echo "  https://hostname.com/confluence/rpc/xmlrpc"
echo ""