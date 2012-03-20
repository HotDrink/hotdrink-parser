/**
 * @fileOverview Expression Parser <p>{@link hotdrink.parser.ExprParser}</p>
 * @author Wonseok Kim
 */

//provides("hotdrink.parser.ExprParser");

//requires("hotdrink.parser.parsec");

/*
 * Expression AST consists of expression nodes which have "type" info.
 * The type info is of enumeration type ExprType.
 * 
 * Depending on the type of node, the content of node can be different.
 * The following list shows all possible types:
 * 
 * case literal: boolean, number, and string literal
 *   {data: literal}
 *   
 * case empty: keyword empty
 *   
 * case var_ref: variable reference
 *   {id: variable_id}
 *   
 * case unary_op: unary operator expression (e.g. !expr1)
 *   {op: operator, expr: expr1}
 * 
 * case binary_op: binary operator expression (e.g. expr1 + expr2)
 *   {op: operator, expr: [expr1, expr2]}
 * 
 * case cond: conditional expression (expr1 ? expr2 : expr3)
 *   {expr: [expr1, expr2, expr3]}
 * 
 * case list: list expression ([expr1, expr2, ...])
 *   {expr: [expr1, expr2, ...]}
 * 
 * case dict: dictionary expression ({id1: expr1, id2: expr2, ...})
 *   {id: [id1, id2, ...], expr: [expr1, expr2, ...]}
 * 
 * case subs: subscript expression (expr1[expr2])
 *   {expr: [expr1, expr2]}
 *   
 * case dot: member access expression for dictionary (expr1.id)
 *   {expr: expr1, id: id}
 *  
 * case call: function call (func_expr(expr1, expr2, ...))
 *   {func: func_expr, params: [expr1, expr2, ...]}
 */
(function() {
  /**
   * @name hotdrink.parser.ExprParser
   * @class Expression Parser.
   */
  var ns = namespace.open("hotdrink.parser.ExprParser");

  /* import */
  var parsec = namespace.open("hotdrink.parser.parsec");
  var choice = parsec.choice;
  var seq = parsec.seq;
  var wseq = parsec.wseq;
  var optional = parsec.optional;
  var many = parsec.many;
  var action = parsec.action;
  var expect = parsec.expect;
  var identifier = parsec.identifier;
  var wlist = parsec.wlist;
  var parens = parsec.parens;

  /**
   * Expression types which represent the type of Expression Nodes.
   * @memberOf hotdrink.parser.ExprParser
   * @constant
   */
  var ExprType = {
    literal : "literal",
    empty : "empty",
    var_ref : "var_ref",
    unary_op : "unary_op",
    binary_op : "binary_op",
    cond : "cond",
    list : "list",
    dict : "dict",
    subs : "subs",
    dot : "dot",
    call : "call"
  };

  /* Expression Nodes */
  var LiteralNode = function (literal) {
    this.type = ExprType.literal;
    this.data = literal;
  };
  var EmptyNode = function () {
    this.type = ExprType.empty;
  };
  var VarRefNode = function (id) {
    this.type = ExprType.var_ref;
    this.id = id;
  };
  var UnaryOpNode = function (op, expr) {
    this.type = ExprType.unary_op;
    this.op = op;
    this.expr = expr;
  };
  var BinaryOpNode = function (op, expr1, expr2) {
    this.type = ExprType.binary_op;
    this.op = op;
    this.expr = [expr1, expr2];
  };
  var CondNode = function (expr1, expr2, expr3) {
    this.type = ExprType.cond;
    this.expr = [expr1, expr2, expr3];
  };
  var ListNode = function (exprs) {
    this.type = ExprType.list;
    this.expr = exprs;
  };
  var DictNode = function (ids, exprs) {
    this.type = ExprType.dict;
    this.id = ids;
    this.expr = exprs;
  };
  var SubsNode = function (expr1, expr2) {
    this.type = ExprType.subs;
    this.expr = [expr1, expr2];
  };
  var DotNode = function (expr1, id) {
    this.type = ExprType.dot;
    this.expr = expr1;
    this.id = id;
  };
  var CallNode = function (func, params) {
    this.type = ExprType.call;
    this.func = func;
    this.params = params;
  };

  /* forward declarations (necessary for cross reference) */
  var expression = function(i) { return expression(i); };
  var unaryExpression = function(i) { return unaryExpression(i); };

  var literal = action(parsec.literal, function (ast) { 
    return new LiteralNode(ast); 
  });

  var empty = action(parsec.token("empty"), function (ast) { 
    return new EmptyNode(); 
  });
  
  /* variable = identifier  */
  var variable = action(identifier, function (ast) {
    return new VarRefNode(ast);
  });

  /* array = "[" [argument_list] "]" */
  var array = action(
      wseq("[", optional(wlist(expression, ","), true), "]"), 
      function (ast) {
        var exprs = ast[1];
        if (exprs === false) exprs = [];
        return new ListNode(exprs);
      });

  /* named_argument = identifier ":" expression */
  var namedArgument = action(
      wseq(identifier, expect(":"), expression), 
      function (ast) { return {id: ast[0], expr: ast[1]}; }
  );

  /* dictionary = "{" [named_argument_list] "}" */
  var dictionary = action(
      wseq("{", optional(wlist(namedArgument, ","), true), "}"), 
      function (ast) {
        var argList = ast[1];
        var ids = [];
        var exprs = [];
        if (argList !== false) { /* non-empty dict */
          for (var i=0; i<argList.length; ++i) {
            ids.push(argList[i].id);
            exprs.push(argList[i].expr);
          }
        }
        return new DictNode(ids, exprs);
      }
  );

  /*
   * primary_expression = number | boolean | string | "empty" 
   *          | array | dictionary | variable | ( "(" expression ")" )
   */
  var primaryExpression = 
    choice(literal, empty, array, dictionary, variable, parens(expression));

  /* postfix_expression = primary_expression 
   * { ("[" expression "]") | ("." identifier) | "(" [argument_list] ")" } 
   */
  var postfixExpression = action(
      wseq(primaryExpression, many( choice( 
          wseq("[", expression, "]"), 
          wseq(".", identifier), 
          wseq("(", optional(wlist(expression, ","), true), ")") 
          ))),
      function (ast) {
        /* ast is an array [ primary_part, many_part ] */
        var e1 = ast[0];
        if (ast.length == 1) return e1; /* primary expr only */
        
        var manypart = ast[1];
        var node;
        for (var i=0; i<manypart.length; ++i) {
          var postfix = manypart[i];
          if (postfix[0] === "[") { /* subscript */
            var e2 = postfix[1];
            node = new SubsNode(e1, e2);
          } else if (postfix[0] === ".") { /* dot */
            var id = postfix[1];
            node = new DotNode(e1, id);
          } else if (postfix[0] === "(") { /* func call */
            var exprs = postfix[1] || [];
            return new CallNode(e1, exprs);
          } else {
            throw "error"; /* never reached */
          }
          e1 = node;
        }
        return node;
      }
  );
  

  /* unary_operator = "+" | "-" | "!" */
  var unary_operator = choice("+", "-", "!");

  /* unary_expression = postfix_expression | (unary_operator unary_expression) */
  var unaryExpression = choice( 
      postfixExpression, 
      action( wseq(unary_operator, unaryExpression),
        function (ast) {
          return new UnaryOpNode(ast[0], ast[1]);
        }
      )
  );
      
  /* helper action for binary operations */
  var binaryOpAction = function (ast) { 
    if (ast.length == 1) return ast[0]; /* first expression only */
    var e1 = ast[0];
    var node;
    for (var i=0; i<ast[1].length; ++i) {
      var op = ast[1][i][0];
      var e2 = ast[1][i][1];
      
      node = new BinaryOpNode(op, e1, e2);
      e1 = node;
    }
    return node;
  };

  /* multiplicative_expression 
   * = unary_expression { ("*" | "/" | "%") unary_expression } */
  var multiplicativeExpression = action(
      wseq(unaryExpression, many( wseq( choice("*", "/", "%"), unaryExpression) ) ),
      binaryOpAction);

  /* additive_expression = multiplicative_expression { ("+" | "-") multiplicative_expression } */
  var additiveExpression = action(
      wseq(multiplicativeExpression,
          many(wseq(choice("+", "-"), multiplicativeExpression))),
          binaryOpAction);

  /* relational_expression = additive_expression { ("<" | ">" | "<=" | ">=") additive_expression } */
  var relationalExpression = action(
      wseq(additiveExpression,
          many( wseq( choice("<=", ">=", "<", ">"), additiveExpression ) )),
          binaryOpAction);

  /* equality_expression = relational_expression { ("==" | "!=") relational_expression } */
  var equalityExpression = action(
      wseq(relationalExpression,
      many( wseq( choice("==", "!="), relationalExpression ) )),
      binaryOpAction);

  /* and_expression = equality_expression { "&&" equality_expression } */
  var andExpression = action(
      wseq(equalityExpression, many( wseq( "&&", equalityExpression) )),
      binaryOpAction);

  /* or_expression = and_expression { "||" and_expression } */
  var orExpression = action(
      wseq(andExpression, many( wseq( "||", andExpression) )),
      binaryOpAction);

  /* expression = or_expression ["?" expression ":" expression] */
  var expression = action(
      wseq(orExpression, 
      optional( wseq(expect("?"), expression, expect(":"), expression))),
      function (ast) {
        if (ast.length == 1) return ast[0];
        var e1 = ast[0];
        var e2 = ast[1][0];
        var e3 = ast[1][1];
        
        return new CondNode(e1,e2,e3);
      });

  /* parse :: string -> ExprAST */
  /**
   * Parse expression and returns the AST of it.
   * @memberOf hotdrink.parser.ExprParser
   * 
   * @param {String} input expression
   * @returns {ExprNode} Expression AST
   */
  var parse = function (input) {
    var state = new parsec.State(input);
    var parser = expression;

    var reply = parser(state);
    if (!reply.ok) {
      var err = parsec.printError(reply.msg);
      throw err;
    }
    if (reply.state.length != 0) {
      var err = "parse error at " + reply.state.pos.toString()
      + ": " + "unexpected \"" + reply.state.toString() + "\"";
      throw err;
    }
    
    return reply.ast;
  };
  
  
  /**
   * Default AST visitor for writing JavaScript code. 
   * @memberOf hotdrink.parser.ExprParser
   * @field
   */
  var DefaultCodeVisitor = Class.create({
    initialize : function () {},
    visit : function (node) {
      switch(node.type) {
      
      case ExprType.empty:
        return this.visitEmpty(node);
      case ExprType.literal:
        return this.visitLiteral(node);
      case ExprType.var_ref:
        return this.visitVarRef(node);
      case ExprType.unary_op:
        return this.visitUnaryOp(node);
      case ExprType.binary_op:
        return this.visitBinaryOp(node);
      case ExprType.cond:
        return this.visitConditional(node);
      case ExprType.list:
        return this.visitList(node);
      case ExprType.dict:
        return this.visitDict(node);
      case ExprType.subs:
        return this.visitSubscript(node);
      case ExprType.dot:
        return this.visitDot(node);
      case ExprType.call:
        return this.visitCall(node);
      default:
        throw "Error: invalid type " + node.type; /* never reached */
      }
    },
    /* visit and wrap the result with parenthesis if required */
    visitAndWrap : function (node) {
      var code = this.visit(node);
      
      switch(node.type) {
      case ExprType.unary_op:
      case ExprType.binary_op:
      case ExprType.cond:
        code = "(" + code + ")";
        break;
      }
      return code;
    },
    visitEmpty : function (node) {
      /* empty -> null */
      return "null";
    },
    visitLiteral : function (node) {
      return node.data;
    },
    visitVarRef : function (node) {
      return node.id;
    },
    visitUnaryOp : function (node) {
      /* op(expr) */
      return node.op + this.visitAndWrap(node.expr);
    },
    visitBinaryOp : function (node) {
      /* (e1) op (e2) */
      var expr = node.expr;
      return this.visitAndWrap(expr[0]) + node.op + this.visitAndWrap(expr[1]);
    },
    visitConditional : function (node) {
      /* (e1) ? (e2) : (e3) */
      var expr = node.expr;
      return this.visitAndWrap(expr[0]) + " ? " + this.visitAndWrap(expr[1])+ " : " 
      + this.visitAndWrap(expr[2]);
    },
    visitList : function (node) {
      /* [e1, e2, ...] */
      var expr = node.expr;
      var code = "[";
      if (expr.length > 0) code += this.visit(expr[0]);
      for (var i=1; i<expr.length; ++i) {
        code += ",";
        code += this.visit(expr[i]);
      }
      code += "]";
      return code;
    },
    visitDict : function (node) {
      /* {id1: e1, id2: e2, ...} */
      var ids = node.id;
      var exprs = node.expr;
      var code = "{";
      if (ids.length > 0) code += ids[0] + ":" + this.visit(exprs[0]);
      for (var i=1; i<ids.length; ++i) {
        code += ",";
        code += ids[i] + ":" + this.visit(exprs[i]);
      }
      code += "}";
      return code;
    },
    visitSubscript : function (node) {
      /* (e1)[e2] */
      var expr = node.expr;
      return this.visitAndWrap(expr[0]) + "[" + this.visit(expr[1]) + "]";
    },
    visitDot : function (node) {
      /* (expr).id */
      return this.visitAndWrap(node.expr) + "." + node.id;
    },
    visitCall : function (node) {
      /* func_expr(e1,e2,...) */
      var expr = node.params;
      var code = this.visit(node.func) + "(";
      if (expr.length > 0) code += this.visit(expr[0]);
      for (var i=1; i<expr.length; ++i) {
        code += ",";
        code += this.visit(expr[i]);
      }
      code += ")";
      return code;
    }
  });
  
  /**
   * Print default JavaScript code from Expression AST.
   * @memberOf hotdrink.parser.ExprParser
   * @param {ExprNode} exprAst AST retrieved from function parse
   * @returns {String} JavaScript code
   */
  var writeJavaScript = function (exprAst) {
    var visitor = new DefaultCodeVisitor();
    return visitor.visit(exprAst);
  };

  ns.ExprType = ExprType;
  ns.primaryExpression = primaryExpression;
  ns.expression = expression;
  ns.parse = parse;
  ns.writeJavaScript = writeJavaScript;
  ns.DefaultCodeVisitor = DefaultCodeVisitor;

})();

