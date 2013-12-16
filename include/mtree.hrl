%%-type data() :: any().
-type hash() :: binary().

-type key() :: any().
-type val() :: hash().
-type mtree_node() :: {node, Key::key(), Left::mtree(), Right::mtree()}.
-type mtree_leaf() :: {leaf, Val::val()}.
-type mtree() :: mtree_node()
	       | mtree_leaf().

%%-type mtree_node() :: {node, Left::mtree(), Left::mtree(), ChildHash::hash()}.
%%-type mtree_leaf() :: {leaf, DataHash::hash(), Data::data()}.
