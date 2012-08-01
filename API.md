# REST API (prefixed with /api/1)

## GET /projects

### Response

    [{
        id            :: integer(),
        name          :: string(),
        remote        :: string(),
        build         :: list(command()),
        notifications :: list(#notification())
    }]

## GET /project/<id>

### Response

    {
        id            :: integer(),
        name          :: string(),
        remote        :: string(),
        build         :: list(command()),
        notifications :: list(#notification())
    }

## GET /project/<id>/build

### Response
    [build] %% See below

## GET /project/<id>/build/<id>

### Response

    {
        id            :: integer(),
        project       :: integer(),
        title         :: string(),
        branch        :: string(),
        revision      :: string(),
        author        :: string(),
        start         :: time(),
        stop          :: time(),
        exit          :: integer(),
        output        :: list(string()),
        tags          :: list(tag())
    }

## POST /project/<id>/build

### Variables

    {
        project_id :: integer(),
        title      :: string(),
        branch     :: string(),
        revision   :: string(),
        author     :: string(),
        tags       :: list(string())
    }

or

    {
        copy   :: integer()
    }

### Response

Same as `GET /project/<id>/build/<id>`
