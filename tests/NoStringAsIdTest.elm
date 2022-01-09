module NoStringAsIdTest exposing (suite)

import NoStringAsId as NoStringAsId
import Review.Test
import Test exposing (Test, describe, test)


error : { message : String -> String, details : List String }
error =
    { message = \fieldName -> "Using String to represent Ids is suboptimal and can lead to confusing errors (" ++ fieldName ++ ")."
    , details =
        [ "You would be better of making a custom type (opaque) to work with your Id"
        , """
        type StuffId = 
            StuffId String
        """
        ]
    }


suite : Test
suite =
    describe "NoStringAsId"
        [ describe "checking record types"
            [ test "should not report anything if there is no record type with member ending with `-Id` or called `id` itself of type String" <|
                \() ->
                    """
module A exposing (..)

type alias StuffWithoutId = 
    { almostAnUUInotD: String
    , identification: String
    }

type alias StuffWithId = 
    { id: StuffWithIdId
    , otherField: (String, String)
    , joiningId: JoiningId
    }
    """
                        |> Review.Test.run NoStringAsId.rule
                        |> Review.Test.expectNoErrors
            , test "should report an error if there is at least a record type with a member ending with `-Id` or called `id` itself of type String" <|
                \() ->
                    """
module A exposing (..)

type alias StuffWithoutId = 
    { almostAnUUInotD: String
    , identification: String
    }

type alias StuffWithId = 
    { id: String
    , otherField: (String, String)
    , joiningId: JoiningId
    }

type alias OtherStuffWithId = 
    { id: OtherStuffWithIdId
    , otherField: (String, String)
    , joiningId: String
    }

type alias ReallyBadStuff = 
    { badStuffId: String
    , otherJoiningId: String
    , otherField: (String, String)
    }

    """
                        |> Review.Test.run NoStringAsId.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = error.message "id"
                                , details = error.details
                                , under = "id: String"
                                }
                            , Review.Test.error
                                { message = error.message "joiningId"
                                , details = error.details
                                , under = """, joiningId: String
    }"""
                                }
                            , Review.Test.error
                                { message = error.message "badStuffId"
                                , details = error.details
                                , under = "badStuffId: String"
                                }
                            , Review.Test.error
                                { message = error.message "otherJoiningId"
                                , details = error.details
                                , under = """, otherJoiningId: String
    ,"""
                                }
                            ]
            ]
        , describe "checking function arguments"
            [ test "should not report anything if there is no functions with an argument ending with `-Id` or called `id` itself of type String" <|
                \() ->
                    """
module A exposing (..)

type alias StuffWithId = 
    { id: StuffWithIdId
    , otherField: (String, String)
    , joiningId: JoiningId
    }

someFunction: StuffWithIdId -> List StuffWithId -> Maybe StuffWithId
somefunction id = 
    Debug.todo ""
    """
                        |> Review.Test.run NoStringAsId.rule
                        |> Review.Test.expectNoErrors
            , test "should report an error if there is at least a function with an argument ending with `-Id` or called `id` itself of type String" <|
                \() ->
                    """
module A exposing (..)

type alias StuffWithId = 
    { correctId: StuffWithIdId
    , otherField: (String, String)
    , joiningId: JoiningId
    }

someFunction: String -> List StuffWithId -> Maybe StuffWithId
somefunction id = 
    Debug.todo ""

someOtherFunction: String -> List StuffWithId -> List StuffWithId
someOtherFunction badJoiningId  = 
    Debug.todo ""



anotherLongLongFunction: Id -> (Id -> Id) -> Id -> String -> String
anotherLongLongFunction id stuffId thatId stringId = 
    Debug.todo ""


    Debug.todo ""

    """
                        |> Review.Test.run NoStringAsId.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = error.message "id"
                                , details = error.details
                                , under = "id"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 11, column = 14 }
                                    , end =
                                        { row = 11
                                        , column = 16
                                        }
                                    }
                            , Review.Test.error
                                { message = error.message "badJoiningId"
                                , details = error.details
                                , under = "badJoiningId"
                                }
                            , Review.Test.error
                                { message = error.message "stringId"
                                , details = error.details
                                , under = "stringId"
                                }
                            ]
            ]
        ]
