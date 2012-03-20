/**
 * @fileOverview View Parser <p>{@link hotdrink.parser.ViewParser}</p>
 * @author Wonseok Kim
 */

//provides("hotdrink.parser.ViewParser");

//requires("hotdrink.parser.parsec");
//requires("hotdrink.parser.ExprParser");

(function () {
  /**
   * @name hotdrink.parser.ViewParser
   * @class View Parser.
   */
  var ns = namespace.open("hotdrink.parser.ViewParser");
  
  /* import */
  var parsec = namespace.open("hotdrink.parser.parsec");
  var ExprParser = namespace.open("hotdrink.parser.ExprParser");
  var token = parsec.token;
  var choice = parsec.choice;
  var seq = parsec.seq;
  var wseq = parsec.wseq;
  var optional = parsec.optional;
  var many = parsec.many;
  var many1 = parsec.many1;
  var action = parsec.action;
  var expect = parsec.expect;
  var identifier = parsec.identifier;
  var wlist = parsec.wlist;
  var ExprType = ExprParser.ExprType;
  var primaryExpression = ExprParser.primaryExpression;
  var unwrap = parsec.unwrap;
  var whiteSpaceOrComment = parsec.whiteSpaceOrComment;

  /* Expression translator */
  var ExprWriter = Class.create(ExprParser.DefaultCodeVisitor, {
    initialize : function () {
    },
    visitVarRef : function (node) {
      /* return as string */
      return "\"" + node.id + "\"";
    }
  });

  var writeJavaScript = function (exprAst) {
    var visitor = new ExprWriter();
    return visitor.visit(exprAst);
  };

  /* Data structures */
  /*
   * WidgetAST :=
   *    type :: String
   *    options :: Object
   *    [children :: WidgetAST[]]
   */
  var WidgetAST = function (/*String*/id, /*OptionsAST*/options, /*WidgetAST[]*/children) {
    this.type = id;
    this.options = {};
    
    for (var i=0; i<options.length; i++) {
      var option = options[i];
      var expr = writeJavaScript(option.expr);
      this.options[option.id] = eval(expr);//TODO handle exception more elegantly
    }
    
    if (children !== false) {
      this.children = children;
    }
  };
  
  var endStatement = token(";");
  
  /* forward declarations (necessary for cross reference) */
  var blockDecl = function (i) { return blockDecl(i); };

  /* named-parameter = identifier [ ":" primary_expression ] */
  var namedParameter = action(
      wseq(identifier, optional(wseq(":", primaryExpression), true)), 
      function (ast) { 
        var id = ast[0];/*string*/
        var expr;/*ExprNode*/
        if (ast[1] === false) {
          /* empty expression means value true */
          expr = {type: ExprType.literal, data: true};/*LiteralNode*/
        } else {
          expr = ast[1][1];
        }
        return {id: /*string*/ id, expr: /*ExprNode*/ expr}; 
      }
  );

  /* named-parameter-list = named-parameter { "," named-parameter } */
  var namedParameterList = wlist(namedParameter, ",");
  
  /* options = "(" [named-parameter-list] ")"
   * returns OptionsAST :: [{id:expr},{id:expr},...] 
   */
  var optionsDecl = action(
      wseq("(", optional(namedParameterList, true), ")"),
      function (ast) {
        var npList = ast[1];/*false if none*/
        if (npList === false) return false;
        return ast[1];
      });
  
  /*
   * widget = identifier [ options ] ( ";" | block )
   * returns WidgetAST :: WidgetAST[]
   */
  var widgetDecl = action(
      wseq(identifier, optional(optionsDecl, true), choice(endStatement, blockDecl)),
      function (ast) {
        var id = ast[0];/*string*/
        var options = ast[1];/* OptionsAST or false if none */
        var children = ast[2];/* ";" or WidgetAST[] */
        if (children === ";") children = false;

        var widget = new WidgetAST(id, options, children);
        return widget;
      });

  
  /*
   * block = "{" { widget } "}"
   * returns WidgetAST[]
   */
  var blockDecl = unwrap(wseq(expect("{"), many(widgetDecl, true), expect("}")));
  
  /*
   * view = "view" [ identifier ] block
   * returns ViewAST
   */
  var viewDecl = action(
      wseq("view", optional(identifier, true), blockDecl),
      function (ast) {
        return ast[2];/* WidgetAST[] */
      });
      
  /* 
   * Top-level parser.
   * returns ViewAST 
   */
  var viewParser = unwrap(seq(viewDecl, whiteSpaceOrComment));

  /**
   * Parse view specification and returns view AST.
   * @memberOf hotdrink.parser.ViewParser
   * 
   * @param {String} view string
   * @returns {ViewAST} View AST
   * @throws {Error} If there is an error in the view.
   */
  var parse = function (view) {
    var state = new parsec.State(view);

    var reply = viewParser(state);
    if (!reply.ok) {
      var err = parsec.printError(reply.msg);
      throw err;
    }
    if (reply.state.length != 0) {
      var err = "parse error at " + reply.state.pos.toString()
      + ": " + "unexpected \"" + reply.state.toString() + "\"";
      throw err;
    }
    
    LOG("view ast:\n" + Object.toJSON(reply.ast));

    return reply.ast;
  };
  ns.parse = parse;

})();
