-module(kha_notification_email).

-include_lib("kha/include/common.hrl").
-include("kha.hrl").

-export([send/4]).

-define(SSMTP_CONFIG, "support/ssmtp.conf").
-define(SSMTP, "ssmtp").
-define(FROM, "Kha").

send(_Project, Title, Content, Args) ->
    case check_args(Args) of
        ok                       -> do_send(Title, Content, Args);
        {error, _Reason} = Error -> Error
    end.

do_send(Title, Content, Args) ->
    Emails = proplists:get_value(emails, Args),
    SelfPath = kha_utils:get_app_path(),
    ConfigPath = filename:join([SelfPath, ?SSMTP_CONFIG]),
    case filelib:is_file(ConfigPath) of
        true ->
            [ do_send(Email, Title, Content, ConfigPath) || Email <- Emails ],
            ok;
        false ->
            ?LOG("Not found ssmtp.conf file!", []),
            {error, not_found_config_ssmtp}
    end.

do_send(Email, Title, Content, ConfigPath) ->
    MailData = mail_template(Email, ?FROM, Title, Content),
    file:write_file("/tmp/kha_temp", MailData),
    Cmd0 = "cat /tmp/kha_temp | ssmtp -C ~s ~s",
    Cmd = io_lib:format(Cmd0, [ConfigPath, Email]),
    ?LOG("Mail: ~s~n", [MailData]),
    ?LOG("CMD: ~s~n", [Cmd]),
    kha_utils:sh(Cmd).

check_args(Args) ->
    case proplists:get_value(emails, Args) of
        undefined -> {error, not_found_emails_field};
        _X        -> ok
    end.
                     
mail_template(To, From, Subject, Contents) ->
    D = "To: ~s~n"
        "From: ~s~n"
        "Subject: ~s~n"
        "~n"
        "~s", %% content
    lists:flatten(io_lib:format(D, [To, From, Subject, Contents])).
