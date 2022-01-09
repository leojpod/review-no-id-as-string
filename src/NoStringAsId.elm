module NoStringAsId exposing (rule)

{-| This library add a simple [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to minimize the use of `String` as id and instead suggest using an opaque type.

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, TypeAnnotation(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| This is the rule you should add to your elm-review process.

For details on how to add a rule to your setup, check [here](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/#configuration) it's quite simple!

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoStringAsId" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


isIdLike : String -> Bool
isIdLike name =
    name == "id" || String.endsWith "id" (String.toLower name)


isStringNode : Node ( ModuleName, String ) -> Bool
isStringNode node =
    (Node.value node == ( [ "Basic" ], "String" ))
        || (Node.value node == ( [], "String" ))


recordDefinitionVisitor : RecordDefinition -> List (Error {})
recordDefinitionVisitor =
    List.filterMap
        (\node ->
            let
                ( fieldName, type_ ) =
                    Node.value node
                        |> Tuple.mapBoth Node.value Node.value

                isString =
                    case type_ of
                        Typed node_ [] ->
                            isStringNode node_

                        _ ->
                            -- let
                            -- _ =
                            -- Debug.log "type_ -> " type_
                            -- in
                            False
            in
            if isIdLike fieldName && isString then
                Just <|
                    Rule.error
                        { message = "Using String to represent Ids is suboptimal and can lead to confusing errors (" ++ fieldName ++ ")."
                        , details =
                            [ "You would be better of making a custom type (opaque) to work with your Id"
                            , """
        type StuffId = 
            StuffId String
        """
                            ]
                        }
                        (Node.range node)

            else
                Nothing
        )


areStringArgsFromTypeAnnocation : List Bool -> Node TypeAnnotation -> List Bool
areStringArgsFromTypeAnnocation argsSoFar typeAnnotationNode =
    case Node.value typeAnnotationNode of
        Typed node [] ->
            isStringNode node :: argsSoFar

        FunctionTypeAnnotation firstNode secondNode ->
            areStringArgsFromTypeAnnocation
                ((case Node.value firstNode of
                    Typed node [] ->
                        isStringNode node

                    _ ->
                        False
                 )
                    :: argsSoFar
                )
                secondNode

        _ ->
            False :: argsSoFar


stringArgumentsFromSignature : Maybe (Node Signature) -> List Int
stringArgumentsFromSignature =
    Maybe.map
        (Node.value
            >> .typeAnnotation
            >> areStringArgsFromTypeAnnocation []
            >> List.reverse
            >> List.indexedMap Tuple.pair
            >> List.filter Tuple.second
            >> List.map Tuple.first
        )
        >> Maybe.withDefault []


functionDeclarationVisitor : Function -> List (Error {})
functionDeclarationVisitor { signature, declaration } =
    let
        idLikeArguments =
            declaration
                |> Node.value
                |> .arguments
                |> List.indexedMap
                    Tuple.pair
                |> List.filterMap
                    (\( idx, patternNode ) ->
                        case Node.value patternNode of
                            VarPattern varName ->
                                if isIdLike varName then
                                    Just ( idx, patternNode, varName )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> Debug.log "idLikeArguments"

        stringArgsIdx : List Int
        stringArgsIdx =
            stringArgumentsFromSignature signature
                |> Debug.log "stringArgsIdx"
    in
    List.filterMap
        (\( idx, node, varName ) ->
            if List.member idx stringArgsIdx then
                Just <|
                    Rule.error
                        { message = "Using String to represent Ids is suboptimal and can lead to confusing errors (" ++ varName ++ ")."
                        , details =
                            [ "You would be better of making a custom type (opaque) to work with your Id"
                            , """
        type StuffId = 
            StuffId String
        """
                            ]
                        }
                        (Node.range node)

            else
                Nothing
        )
        idLikeArguments


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.AliasDeclaration { typeAnnotation } ->
            case Node.value typeAnnotation of
                Record recordDefinition ->
                    recordDefinitionVisitor recordDefinition

                GenericRecord _ recordDefinition ->
                    recordDefinitionVisitor <| Node.value recordDefinition

                _ ->
                    []

        Declaration.FunctionDeclaration functionDeclaration ->
            functionDeclarationVisitor functionDeclaration

        _ ->
            []
