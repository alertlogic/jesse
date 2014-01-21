%%%-----------------------------------------------------------------------------
%% @doc Parsing and combining URIs.
%%
%% This module follows the rules of RFC3986. It provides interface for
%% parsing and rendering URIs from/to their string representation. Also
%% this module resolves relative URIs against given base. This module is
%% completely side-effect free and, in particular, throws no exceptions.
%% @end
%%
%% @author Mikhail Mitrofanov <migmit@gmail.com>
%% @copyright 2013 Alert Logic, Inc.
%%%-----------------------------------------------------------------------------
%%%=============================================================================

-module(jesse_uri).

-export([parse/1, render/1, is_absolute/1, is_local/1, resolve/2]).
-export([get_fragment/1, set_fragment/2]).

-type(scheme() :: string()).
-type(userinfo() :: no_userinfo | string()).
-type(host() :: string()).
-type(uport() :: no_port | integer()).
-type(authority() :: {userinfo(), host(), uport()}).
-type(segment() :: string()).
-type(path() :: [segment()]).
-type(uquery() :: no_query | string()).
-type(fragment() :: no_fragment | string()).
-type(hier() ::
    {with_host, authority(), path()} |
    {absolute, path()} |
    {relative, path()} |
    empty
).
-type(absolute() :: {scheme(), hier(), uquery(), fragment()}).
-type(relative() :: {hier(), uquery(), fragment()}).
-type(uri() :: absolute() | relative()).

-spec(get_fragment(URI :: uri()) -> fragment()).
get_fragment({_Scheme, _Hier, _Query, Fragment}) -> Fragment;
get_fragment({_Hier, _Query, Fragment}) -> Fragment.

-spec(set_fragment(URI :: uri(), Fragment :: fragment()) -> uri()).
set_fragment({Scheme, Hier, Query, _Fragment}, Fragment) ->
    {Scheme, Hier, Query, Fragment};
set_fragment({Hier, Query, _Fragment}, Fragment) ->
    {Hier, Query, Fragment}.

-spec(index_of(Input :: list(), Elt :: term()) -> not_found | integer).
index_of(Input, Elt) -> index_of(Input, Elt, 1).

-spec(index_of(Input :: list(), Elt :: term(), N :: integer) ->
    not_found | integer).
index_of([], _Elt, _N) -> not_found;
index_of([C | Rest], Elt, N) ->
    if
        C =:= Elt -> N;
        true -> index_of(Rest, Elt, N+1)
    end.

-spec(chop(Input :: list(), Sep :: term()) -> nothing | {list(), list()}).
chop(Input, Sep) ->
    case index_of(Input, Sep) of
        not_found -> nothing;
        N ->
            {Start, End} = lists:split(N-1, Input),
            {Start, tl(End)}
    end.

-spec(split_by(Input :: list(), Sep :: term()) -> [list()]).
split_by(Input, Sep) ->
    case chop(Input, Sep) of
        nothing -> [Input];
        {Start, End} -> [Start | split_by(End, Sep)]
    end.

-spec(chop_fragment(Input :: string()) -> {string(), fragment()}).
chop_fragment(Input) ->
    case chop(Input, $#) of
        nothing -> {Input, no_fragment};
        {Start, End} -> {Start, End}
    end.

-spec(chop_query(Input :: string()) -> {string(), uquery()}).
chop_query(Input) ->
    case chop(Input, $?) of
        nothing -> {Input, no_query};
        {Start, End} -> {Start, End}
    end.

-spec(parse_authority(Input :: string()) ->
    {error, term()} | {ok, authority()}).
parse_authority(Input) ->
    {UserInfo, HostAndPort} =
        case chop(Input, $@) of
            nothing -> {no_userinfo, Input};
            {Start, End} -> {Start, End}
        end,
    case chop(HostAndPort, $:) of
        nothing -> {ok, {UserInfo, HostAndPort, no_port}};
        {Host, PortString} ->
            case string:to_integer(PortString) of
                {Port, ""} -> {ok, UserInfo, Host, Port};
                _ -> {error, {invalid_port, PortString}}
            end
    end.

-spec(parse_hier(Input :: string()) -> {error, term()} | {ok, hier()}).
parse_hier([]) -> {ok, empty};
parse_hier([$/, $/ | Rest]) ->
    {Authority, Path} =
        case chop(Rest, $/) of
            nothing -> {Rest, []};
            {Start, End} -> {Start, split_by(End, $/)}
        end,
    case parse_authority(Authority) of
        {error, Reason} -> {error, Reason};
        {ok, Auth} -> {ok, {with_host, Auth, Path}}
    end;
parse_hier([$/ | Rest]) -> {ok, {absolute, split_by(Rest, $/)}};
parse_hier(Path) -> {ok, {relative, split_by(Path, $/)}}.

-spec(parse_absolute(Input :: string()) -> {error, term()} | {ok, absolute()}).
parse_absolute(Input) ->
    {BeforeFragment, Fragment} = chop_fragment(Input),
    {BeforeQuery, Query} = chop_query(BeforeFragment),
    case chop(BeforeQuery, $:) of
        nothing -> {error, no_scheme};
        {Scheme, HierString} ->
            case parse_hier(HierString) of
                {error, Reason} -> {error, Reason};
                {ok, Hier} ->
                    {ok, {Scheme, Hier, Query, Fragment}}
            end
    end.

-spec(parse_relative(Input :: string()) -> {error, term()} | {ok, relative()}).
parse_relative(Input) ->
    {BeforeFragment, Fragment} = chop_fragment(Input),
    {BeforeQuery, Query} = chop_query(BeforeFragment),
    case parse_hier(BeforeQuery) of
        {error, Reason} -> {error, Reason};
        {ok, Hier} -> {ok, {Hier, Query, Fragment}}
    end.

-spec(is_absolute(URI :: uri()) -> boolean()).
is_absolute({_Scheme, _Hier, _Query, _Fragment}) -> true;
is_absolute({_Hier, _Query, _Fragment}) -> false.

-spec(is_local(URI :: uri()) -> boolean()).
is_local({empty, no_query, _Fragment}) -> true;
is_local({_Scheme, _Hier, _Query, _Fragment}) -> false;
is_local({_Hier, _Query, _Fragment}) -> false.

-spec(parse(Input :: string()) -> {error, term()} | {ok, uri()}).
parse(Input) ->
    case parse_absolute(Input) of
        {ok, Absolute} -> {ok, Absolute};
        {error, _} -> parse_relative(Input)
    end.

-spec(render_authority(Authority :: authority()) -> string()).
render_authority({UserInfo, Host, Port}) ->
    UserInfoString =
        case UserInfo of
            no_userinfo -> "";
            _ -> string:concat(UserInfo, "@")
        end,
    PortString =
        case Port of
            no_port -> "";
            _ -> [$: | integer_to_list(Port)]
        end,
    lists:concat([UserInfoString, Host, PortString]).

-spec(render_path(Path :: path()) -> string()).
render_path([]) -> "";
render_path([Segment]) -> Segment;
render_path([Segment | Rest]) -> lists:concat([Segment, $/, render_path(Rest)]).

-spec(render_hier(Hier :: hier()) -> string()).
render_hier({with_host, Authority, []}) ->
    lists:concat(["//", render_authority(Authority)]);
render_hier({with_host, Authority, Path}) ->
    lists:concat(["//", render_authority(Authority), "/", render_path(Path)]);
render_hier({absolute, Path}) -> [$/ | render_path(Path)];
render_hier({relative, Path}) -> render_path(Path);
render_hier(empty) -> "".

-spec(render_relative(URI :: relative()) -> string()).
render_relative({Hier, Query, Fragment}) ->
    QueryString =
        case Query of
            no_query -> "";
            _ -> [$? | Query]
        end,
    FragmentString =
        case Fragment of
            no_fragment -> "";
            _ -> [$# | Fragment]
        end,
    lists:concat([render_hier(Hier), QueryString, FragmentString]).

-spec(render_absolute(URI :: absolute()) -> string()).
render_absolute({Scheme, Hier, Query, Fragment}) ->
    lists:concat([Scheme, ":", render_relative({Hier, Query, Fragment})]).

-spec(render(URI :: uri()) -> string()).
render(URI) ->
    IsAbs = is_absolute(URI),
    if
        IsAbs -> render_absolute(URI);
        true -> render_relative(URI)
    end.

-spec(merge_paths(BPath :: path(), Path :: path()) -> path()).
merge_paths([], Path) -> Path;
merge_paths([_Segment], Path) -> Path;
merge_paths([Segment | Rest], Path) -> [Segment | merge_paths(Rest, Path)].

-spec(remove_dots(Path :: path()) -> path()).
remove_dots(Path) ->
    try
        remove_dots(Path, [])
    catch
        throw:overflow -> throw({overflow, Path})
    end.

-spec(remove_dots(Path :: path(), Buffer :: [segment()]) -> path()).
remove_dots([], Buffer) -> lists:reverse(Buffer);
remove_dots(["." | Path], Buffer) -> remove_dots(Path, Buffer);
remove_dots([".." | Path], [_ | Buffer]) -> remove_dots(Path, Buffer);
remove_dots([".." | _], []) -> throw(overflow);
remove_dots([Segment | Path], Buffer) -> remove_dots(Path, [Segment | Buffer]).

-spec(resolve(Base :: absolute(), URI :: uri()) ->
    {ok, uri()} | {error, term()}).
resolve(_BaseURI, {_Scheme, _Hier, _Query, _Fragment} = URI) -> {ok, URI};
resolve({Scheme, Hier, _Query, _Fragment}, {RHier, Query, Fragment}) ->
    try
        {ok, {Scheme, join_hier(Hier, RHier), Query, Fragment}}
    catch
        throw:Err -> {error, Err}
    end.

-spec(join_hier(BaseHier :: hier(), Hier :: hier()) -> hier()).
join_hier(_BaseHier, {with_host, _Authority, _Path} = Hier) -> Hier;
join_hier({with_host, Authority, _Path}, {absolute, Path}) ->
    {with_host, Authority, remove_dots(Path)};
join_hier(_BaseHier, {absolute, Path}) -> {absolute, remove_dots(Path)};
join_hier({with_host, Authority, BPath}, {relative, Path}) ->
    {with_host, Authority, remove_dots(merge_paths(BPath, Path))};
join_hier({AbsOrRel, BPath}, {relative, Path}) ->
    {AbsOrRel, remove_dots(merge_paths(BPath, Path))};
join_hier(empty, {relative, _Path} = URI) -> URI;
join_hier(BaseHier, empty) -> BaseHier.