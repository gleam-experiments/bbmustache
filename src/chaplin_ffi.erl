-module(chaplin_ffi).

-export([try_catch/1, argument/1]).

try_catch(F) ->
    try {ok, F()}
    catch 
        _:file_not_found ->
            {error, file_not_found};

        _:{incorrect_format, unclosed_tag} ->
            {error, unclosed_tag};

        _:{incorrect_format, {unsupported_tag, Tag}} ->
            {error, {unsupported_tag, Tag}};

        _:{incorrect_format, delimiters_may_not_contain_equals} ->
            {error, invalid_delimiters};

        _:{incorrect_format, delimiters_may_not_contain_whitespaces} ->
            {error, invalid_delimiters};

        _:{incorrect_format, {section_is_incorrect, Tag}} ->
            {error, {incorrect_section, Tag}};

        _:{incorrect_format, {section_end_tag_not_found, Tag}} ->
            {error, {unclosed_section, Tag}}
    end.

argument(V) ->
    V.
