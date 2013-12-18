
-type hash() :: binary().

-type val() :: hash() | nil.
-type mtree_node() :: {node, Val::val(), Left::mtree(), Right::mtree()}.
-type mtree_leaf() :: {leaf, Val::val()}.
-type mtree() :: mtree_node()
	       | mtree_leaf().

-type pos() :: l | r.
-type pos_list() :: [pos()].
-type pos_bin() :: binary().

