#include<cstdio>/*{{{*/
#include<cstdlib>
#include<cstring>
#include<cctype>
#include<string>
#include<vector>
#include<set>
#include<map>
#include<algorithm>
using namespace std;
#define TC token_cur
#define TF TC->first
#define TS TC->second/*}}}*/
char to_string[][3] = {"+", "-", "*", "/", "==", "<=", "!=", "<", ">=", ">", "%", "@", "^", "&", "|", "!", "<<", ">>", "^"};/*{{{*//*}}}*/

#define INDENT_STEP 4

void error() {/*{{{*/
    puts("error");
    exit(-1);
}/*}}}*/
typedef pair<int, int> PII;
class Parser {
    struct Vars {/*{{{*/
        vector<string> id;
        int d;
        vector<int> l, r;
        int type;
    };/*}}}*/

    static const int OP = 0;/*{{{*/
    static const int PROG = 1;
    static const int FUNC = 2;
    static const int BEGIN = 3;
    static const int END = 4;
    static const int PROC = 5;
    static const int VAR = 6;
    static const int CONST = 7;
    static const int ARRAY = 8;
    static const int IF = 9;
    static const int THEN = 10;
    static const int ELSE = 11;
    static const int CASE = 12;
    static const int FOR = 13;
    static const int TO = 14;
    static const int DO = 15;
    static const int REP = 16;
    static const int UNTIL = 17;
    static const int WHILE = 18;
    static const int NIL = 19;
    static const int ID = 20;
    static const int COL = 21;
    static const int LP = 22;
    static const int RP = 23;
    static const int LB = 24;
    static const int RB = 25;
    static const int COM = 26;
    static const int SIM = 27;
    static const int ASSIGN = 28;/*}}}*/
    static const int INT = -1;/*{{{*/
    static const int DOUBLE = -2;
    static const int CHAR = -3;
    static const int BOOL = -4;
    static const int STRING = -5;/*}}}*/
    static const int ADD = 0;/*{{{*/
    static const int SUB = 1;
    static const int MUL = 2;
    static const int DIV = 3;
    static const int EQ = 4;
    static const int LE = 5;
    static const int NE = 6;
    static const int LT = 7;
    static const int GE = 8;
    static const int GT = 9;
    static const int MOD = 10;
    static const int ADDRESS = 11;
    static const int VALUE = 12;
    static const int AND = 13;
    static const int OR = 14;
    static const int NOT = 15;
    static const int SHL = 16;
    static const int SHR = 17;
    static const int XOR = 18;/*}}}*/
    char buffer[1024];/*{{{*/
    FILE* out_file;
    int indent_depth;
    int type;
    string fun_id_cur;
    map<string, int> id_map;
    vector<string> id_map_rev;
    set<int> all_type;
    string content;
    map<string, vector<int> > global_st, local_st;
    vector<PII> tokens;
    vector<PII>::iterator TC;/*}}}*/
    void init() {/*{{{*/
        fprintf(out_file, "#include<iostream>\n");
        fprintf(out_file, "#include<cstdio>\n");
        fprintf(out_file, "#include<cstdlib>\n");
        fprintf(out_file, "#include<cstring>\n");
        fprintf(out_file, "#include<cmath>\n");
        fprintf(out_file, "#include<string>\n");
        fprintf(out_file, "using namespace std;\n\n");
    }/*}}}*/
    string fun_io(int type = 0) {/*{{{*/
        string ret = type ? "cout" : "cin";
        if (TF == LP) {
            ++TC;
            while(TF != RP) {
                if (TF == COM) {
                    ++TC;
                } else if (TF == ID || TF == LP) {
                    ret += type ? " << " : " >> ";
                    ret += exp(1);
                }
            }
            ++TC;
        } else if (type != 2) {
            ret += " >> endl";
        }
        if (type == 2) {
            ret += " << endl";
        }
        return ret;
    }/*}}}*/
    string fun_fill() {/*{{{*/
        string ret = "memset(";
        if (TF == LP) {
            ++TC;
            if (TF == ID || TF == LP) {
                ret += exp(1);
            } else {
                error();
            }
            if (TF == COM) {
                ++TC;
            }
            string tmp = "";
            if (TF == ID || TF == LP) {
                tmp += exp(1);
            } else {
                error();
            }
            if (TF == COM) {
                ++TC;
            }
            if (TF == ID || TF == LP) {
                ret += ", ";
                ret += exp();
                ret += ", ";
                ret += tmp;
                ret += ")";
            } else {
                error();
            }
            if (TF == RP) {
                ++TC;
            } else {
                error();
            }
        } else {
            error();
        }
        return ret;
    }/*}}}*/
    string get_id() {/*{{{*/
        string id = id_map_rev[TS];
        if (id == "trunc") {
            id = "(int)trunc";
        }
        string ret = id;
        string id_s = "";
        for (int i = id[0] == '-' ? 1 : 0; i < id.length(); ++i) {
            id_s += id[i];
        }
        ++TC;
        int defined = 0;
        if (local_st.find(id_s) != local_st.end()) {
            defined = 1;
        } else if (global_st.find(id_s) != global_st.end()) {
            defined = 2;
        }
        if (!defined) {
            return ret;
        }
        vector<int>& def = defined == 1 ? local_st.find(id_s)->second : global_st.find(id_s)->second;
        if (TF == LB) {
            ret += '[';
            ++TC;
            int d = 0;
            while(1) {
                if (TF == ID || TF == LP) {
                    ++d;
                    ret += exp(1);
                    char tmp[20] = {0};
                    int t = d > def[0] ? 1 : def[d];
                    if (t != 0) {
                        sprintf(tmp, " %c %d", t > 0 ? '-' : '+', abs(t));
                    }
                    ret += tmp;
                } else {
                    error();
                }
                if (TF == RB) {
                    break;
                } else if (TF == COM) {
                    ret += "][";
                    ++TC;
                } else {
                    error();
                }
            }
            ++TC;
            ret += "]";
        }
        return ret;
    }/*}}}*/
    void indent(string& s) {/*{{{*/
        for (int i = 0; i < indent_depth * INDENT_STEP; ++i) {
            s += " ";
        }
    }/*}}}*/
    string exp(bool com = false) {/*{{{*/
        string ret = "";
        int tot = 0;
        while(1) {
            if (TF == LP || TF == RP || TF == OP) {
                if (TF == LP) {
                    ++tot;
                } else if (TF == RP) {
                    if (--tot < 0) {
                        return ret;
                    }
                }
                if (TF == OP) {
                    ret += " ";
                }
                ret += TF == LP ? "(" : TF == RP ? ")" : to_string[TS];
                if (TF == OP) {
                    ret += " ";
                }
                ++TC;
            } else if ((TF == COM && com && !tot) || TF == RB || TF == THEN || TF == ELSE || TF == SIM || TF == DO || TF == TO) {
                if (TF == SIM) ++TC;
                return ret;
            } else if (TF == ID) {
                ret += get_id();
            } else if (TF == COM) {
                ret += ", ";
                ++TC;
            } else {
                error();
            }
        }
        return ret;
    }/*}}}*/
    string match_statement(bool noline = false) {/*{{{*/
        string ret = "";
        bool flag_sim = false;
        indent(ret);
        if (TF == FOR) {
            flag_sim = true;
            ret += "for (";
            ++TC;
            string lvar = "";
            if (TF == ID) {
                lvar = get_id();
                ret += lvar;
                ret += " = ";
            } else {
                error();
            }
            if (TF != ASSIGN) {
                error();
            }
            ++TC;
            if (TF == ID || TF == LP) {
                ret += exp();
                ret += "; ";
            } else {
                error();
            }
            bool flag = false;
            if (TF == TO) {
                if (TS == 1) {
                    flag = true;
                }
                ret += lvar;
                ret += flag ? " <= " : " >= ";
                ++TC;
            } else {
                error();
            }
            if (TF == ID || TF == LP) {
                ret += exp();
                ret += "; ";
                ret += flag ? "++" : "--";
                ret += lvar;
            }
            ret += ")";
            if (TF == DO) {
                ++TC;
            }
            bool tmp = false;
            if (TF != BEGIN) {
                ret += "\n";
                indent(ret);
                ret += "{";
                ++indent_depth;
                tmp = true;
            }
            ret += "\n";
            ret += match_statement(1);
            if (tmp) {
                --indent_depth;
                ret += "\n";
                indent(ret);
                ret += "}";
            }
        } else if (TF == IF) {
            flag_sim = true;
            ret += "if (";
            ++TC;
            ret += exp();
            ret += ")";
            if (TF == THEN) {
                ++TC;
            } else {
                error();
            }
            ret += "\n";
            bool tmp = false;
            if (TF != BEGIN) {
                indent(ret);
                ret += "{";
                ++indent_depth;
                tmp = true;
                ret += "\n";
            }
            ret += match_statement(1);
            if (tmp) {
                --indent_depth;
                ret += "\n";
                indent(ret);
                ret += "}";
            }
            if (TF == ELSE) {
                ret += "\n";
                indent(ret);
                ret += "else";
                ret += "\n";
                ++TC;
                tmp = false;
                if (TF != BEGIN) {
                    indent(ret);
                    ret += "{";
                    ++indent_depth;
                    tmp = true;
                    ret += "\n";
                }
                ret += match_statement(1);
                if (tmp) {
                    --indent_depth;
                    ret += "\n";
                    indent(ret);
                    ret += "}";
                }
            }
        } else if (TF == ID) {
            string id = get_id();
            if (id == "read" || id == "readln") {
                ret += fun_io();
            } else if (id == "write" || id == "writeln") {
                ret += fun_io(id == "write" ? 1 : 2);
            } else if (id == "fillchar") {
                ret += fun_fill();
            } else if (id == "halt") {
                ret += "exit(0)";
            } else if (id == "exit") {
                ret += "return";
                if (type) {
                    ret += " result";
                }
            } else {
                if (TF == ASSIGN && id == fun_id_cur) {
                    ret += "result";
                } else {
                    ret += id;
                }
                if (TF == ASSIGN) {
                    ret += " = ";
                    ++TC;
                    ret += exp();
                } else if (TF == LP) {
                    ret += exp();
                } else if ((TF == SIM  || TF == END || TF == ELSE) && id != "break" && id != "continue") {
                    ret += "()";
                }
            }
        } else if (TF == REP) {
            ret += "do\n";
            indent(ret);
            ret += "{\n";
            ++indent_depth;
            ++TC;
            while (TF != UNTIL) {
                ret += match_statement();
            }
            --indent_depth;
            indent(ret);
            ret += "} ";
            ret += "while (!(";
            ++TC;
            ret += exp();
            ret += "))";
        } else if (TF == WHILE) {
            flag_sim = true;
            ret += "while (";
            ++TC;
            ret += exp();
            ret += ")";
            if (TF == DO) {
                ++TC;
                ret += "\n";
                bool tmp = false;
                if (TF != BEGIN) {
                    indent(ret);
                    ret += "{";
                    ++indent_depth;
                    tmp = true;
                    ret += "\n";
                }
                ret += match_statement(1);
                if (tmp) {
                    --indent_depth;
                    ret += "\n";
                    indent(ret);
                    ret += "}";
                }
            } else {
                error();
            }
        } else if (TF == BEGIN) {
            flag_sim = true;
            ret += "{";
            ++indent_depth;
            ret += "\n";
            ++TC;
            while (TF != END) {
                ret += match_statement();
            }
            --indent_depth;
            indent(ret);
            ret += "}";
            ++TC;
        }
        while (TF == SIM) {
            ++TC;
        }
        if (!flag_sim) {
            ret += ";";
        }
        if (!noline) {
            ret += "\n";
        }
        return ret;
    }/*}}}*/
    bool is_type(const int& id) {/*{{{*/
        return id < 6 || find(all_type.begin(), all_type.end(), id) != all_type.end();
    }/*}}}*/
    int get_type(Vars& ret) {/*{{{*/
        ret.d = 0;
        if (TF == ARRAY) {
            ++TC;
            if (TF == LB) {
                ++TC;
            } else {
                error();
            }
            while(1) {
                ++ret.d;
                for (int i = 0; i < 2; ++i) {
                    if (TF == ID) {
                        vector<int>& t = i ? ret.r : ret.l;
                        t.push_back(atoi(id_map_rev[TS].c_str()));
                        ++TC;
                    } else {
                        error();
                    }
                }
                if (TF != COM) {
                    break;
                } else {
                    ++TC;
                }
            }
            if (TF == RB) {
                ++TC;
            } else {
                error();
            }
        }
        if (TF == ID) {
            int type_id = TS; 
            ++TC;
            if (is_type(type_id)) return type_id;
            error();
        } else {
            error();
        }
        return -1;
    }/*}}}*/
    Vars get_def() {/*{{{*/
        Vars ret;
        while(TF == ID) {
            ret.id.push_back(id_map_rev[TS]);
            ++TC;
            if (TF == COL) {
                ++TC;
                ret.type = get_type(ret);
                break;
            } else if (TF == COM) {
                ++TC;
            } else {
                error();
            }
        }
        return ret;
    }/*}}}*/
    void build_st(bool local = false) {/*{{{*/
        string tmp = "";
        indent(tmp);
        if (!local) {
            global_st.clear();
        }
        map<string, vector<int> >& st = local ? local_st : global_st;
        while (TF == ID) {/*{{{*/
            Vars var = get_def();
            int sz = var.id.size();
            fprintf(out_file, "%s", tmp.c_str());
            if (!var.d) {
                string id = var.id[0];
                if (st.find(id) != st.end()) {
                    error();
                }
                fprintf(out_file, "%s %s", id_map_rev[var.type].c_str(), id.c_str());
                vector<int> tmp;
                tmp.push_back(0);
                st[id] = tmp;
                for (int i = 1; i < sz; ++i) {
                    id = var.id[i];
                    if (st.find(id) != st.end()) {
                        error();
                    }
                    fprintf(out_file, ", %s", id.c_str());
                    st[id] = tmp;
                }
            } else {
                fprintf(out_file, "%s", id_map_rev[var.type].c_str());
                for (int i = 0; i < sz; ++i) {
                    string id = var.id[i];
                    if (st.find(id) != st.end()) {
                        error();
                    }
                    fprintf(out_file, "%s%s", i == 0 ? " " : ", ", id.c_str());
                    vector<int> tmp;
                    tmp.push_back(var.d);
                    for (int j = 0; j < var.d; ++j) {
                        fprintf(out_file, "[%d]", var.r[j] - var.l[j] + 1);
                        tmp.push_back(var.l[j]);
                    }
                    st[id] = tmp;
                }
            }
            fprintf(out_file, ";\n");
            while(TF == SIM) {
                ++TC;
            }
        }/*}}}*/
    }/*}}}*/
    string get_word(int& now) {/*{{{*/
        string ret = "";
        char ch = content[now];
        if (ch == '\'' || ch == '\"') {
            ret += ch;
            char tmp;
            while((tmp = content[++now]) != ch) {
                ret += tmp;
            }
            ret += ch;
            ++now;
            int l = ret.length();
            if (ret[0] == '\'' && l > 3) {
                ret[0] = ret[l - 1] = '\"';
            }
            return ret;
        }
        if (content[now] == '-') {
            ++now;
            ret += '-';
        }
        while(isdigit(content[now]) || isalpha(content[now]) || content[now] == '_' || content[now] == '.' && isdigit(content[now - 1]) && isdigit(content[now + 1])) {
            ret += content[now++];
        }
        return ret;
    }/*}}}*/
    void to_tokens() {/*{{{*/
        string tmp[] = {"int", "double", "char", "bool", "string", "long long"};
        for (int i = 0; i < 6; ++i) {
            id_map[tmp[i]] = i;
            id_map_rev.push_back(tmp[i]);
        }
        int now = 0;
        int tot_length = content.length();
        while(now < tot_length) {
            char ch = content[now];
            bool minus = false;
            if (ch == ' ' || ch == '\n' || ch == '\r') {
                ;
            } else if (ch == ';') {
                tokens.push_back(make_pair(SIM, -1));
            } else if (ch == ',') {
                tokens.push_back(make_pair(COM, -1));
            } else if (ch == '+') {
                tokens.push_back(make_pair(OP, ADD));
            } else if (ch == '-') {
                int tmp = 0;
                if (!tokens.empty() && ((tmp = tokens.back().first) == ID || tmp == RP || tmp == RB)) {
                    tokens.push_back(make_pair(OP, SUB));
                    minus = true;
                }
            } else if (ch == '*') {
                tokens.push_back(make_pair(OP, MUL));
            } else if (ch == '/') {
                tokens.push_back(make_pair(OP, DIV));
            } else if (ch == '=') {
                tokens.push_back(make_pair(OP, EQ));
            } else if (ch == '(') {
                tokens.push_back(make_pair(LP, -1));
            } else if (ch == ')') {
                tokens.push_back(make_pair(RP, -1));
            } else if (ch == '[') {
                tokens.push_back(make_pair(LB, -1));
            } else if (ch == ']') {
                tokens.push_back(make_pair(RB, -1));
            } else if (ch == '<') {
                if (content[now + 1] == '=') {
                    ++now;
                    tokens.push_back(make_pair(OP, LE));
                } else if (content[now + 1] == '>') {
                    ++now;
                    tokens.push_back(make_pair(OP, NE));
                } else {
                    tokens.push_back(make_pair(OP, LT));
                }
            } else if (ch == '>') {
                if (content[now + 1] == '=') {
                    ++now;
                    tokens.push_back(make_pair(OP, GE));
                } else {
                    tokens.push_back(make_pair(OP, GT));
                }
            } else if (ch == ':') {
                if (content[now + 1] == '=') {
                    ++now;
                    tokens.push_back(make_pair(ASSIGN, -1));
                } else {
                    tokens.push_back(make_pair(COL, -1));
                }
            } else if (ch == '@') {
                tokens.push_back(make_pair(OP, ADDRESS));
            } else if (ch == '^') {
                tokens.push_back(make_pair(OP, VALUE));
            }
            if (isalpha(ch) || (!minus && ch == '-') || ch == '_' || isdigit(ch) || ch == '\'' || ch == '\"') {
                string s = get_word(now);
                if (s == "program") {
                    tokens.push_back(make_pair(PROG, -1));
                } else if (s == "function") {
                    tokens.push_back(make_pair(FUNC, -1));
                } else if (s == "begin") {
                    tokens.push_back(make_pair(BEGIN, -1));
                } else if (s == "end") {
                    tokens.push_back(make_pair(END, -1));
                } else if (s == "procedure") {
                    tokens.push_back(make_pair(PROC, -1));
                } else if (s == "var") {
                    tokens.push_back(make_pair(VAR, -1));
                } else if (s == "const") {
                    tokens.push_back(make_pair(CONST, -1));
                } else if (s == "array") {
                    tokens.push_back(make_pair(ARRAY, -1));
                } else if (s == "if") {
                    tokens.push_back(make_pair(IF, -1));
                } else if (s == "then") {
                    tokens.push_back(make_pair(THEN, -1));
                } else if (s == "else") {
                    tokens.push_back(make_pair(ELSE, -1));
                } else if (s == "case") {
                    tokens.push_back(make_pair(CASE, -1));
                } else if (s == "for") {
                    tokens.push_back(make_pair(FOR, -1));
                } else if (s == "to") {
                    tokens.push_back(make_pair(TO, 1));
                } else if (s == "downto") {
                    tokens.push_back(make_pair(TO, -1));
                } else if (s == "do") {
                    tokens.push_back(make_pair(DO, -1));
                } else if (s == "repeat") {
                    tokens.push_back(make_pair(REP, -1));
                } else if (s == "until") {
                    tokens.push_back(make_pair(UNTIL, -1));
                } else if (s == "while") {
                    tokens.push_back(make_pair(WHILE, -1));
                } else if (s == "and") {
                    tokens.push_back(make_pair(OP, AND));
                } else if (s == "div") {
                    tokens.push_back(make_pair(OP, DIV));
                } else if (s == "mod") {
                    tokens.push_back(make_pair(OP, MOD));
                } else if (s == "not") {
                    tokens.push_back(make_pair(OP, NOT));
                } else if (s == "or") {
                    tokens.push_back(make_pair(OP, OR));
                } else if (s == "xor") {
                    tokens.push_back(make_pair(OP, XOR));
                } else if (s == "nil") {
                    tokens.push_back(make_pair(NIL, -1));
                } else if (s == "shl") {
                    tokens.push_back(make_pair(OP, SHL));
                } else if (s == "shr") {
                    tokens.push_back(make_pair(OP, SHR));
                } else if (s == "of") {
                } else if (s == "integer" || s == "longint" || s == "shortint" || s == "word" || s == "byte" || s == "dword") {
                    tokens.push_back(make_pair(ID, 0));
                } else if (s == "real" || s == "single" || s == "double" || s == "extended") {
                    tokens.push_back(make_pair(ID, 1));
                } else if (s == "char") {
                    tokens.push_back(make_pair(ID, 2));
                } else if (s == "boolean") {
                    tokens.push_back(make_pair(ID, 3));
                } else if (s == "string" || s == "ansistring") {
                    tokens.push_back(make_pair(ID, 4));
                } else if (s == "int64" || s == "qword") {
                    tokens.push_back(make_pair(ID, 5));
                } else {
                    map<string, int>::iterator t = id_map.find(s);
                    int u = 0;
                    if (t == id_map.end()) {
                        u = id_map[s] = id_map.size() - 1;
                        id_map_rev.push_back(s);
                    } else {
                        u = t->second;
                    }
                    tokens.push_back(make_pair(ID, u));
                }
                --now;
            }
            ++now;
        }
    }/*}}}*/
    void read_in(FILE* in) {/*{{{*/
        while(fgets(buffer, 1000, in)) {
            content += buffer;
        }
        int len = content.length();
        int depth = 0;
        for (int i = 0; i < len; ++i) {
            if (!depth) {
                content[i] = tolower(content[i]);
            }
            if (content[i] == '\"' || content[i] == '\'' && ((i == 0) || content[i - 1] != '\\')) {
                depth ^= content[i] == '\"' ? 1 : 2;
            }
            if (i + 1 < len && content[i] == ']' && content[i + 1] == '[') {
                content[i] = ',';
                content[++i] = ' ';
            }
        }
    }/*}}}*/
    void match_function() {/*{{{*/
        string ret_type = "";
        while(TF != BEGIN) {
            type = TF == FUNC;
            if (TF == FUNC || TF == PROC) {
                ++TC;
                if (TF != ID) {
                    error();
                }
                map<string, vector<int> >& st = local_st;
                st.clear();
                string fun_id = id_map_rev[TS];
                fun_id_cur = fun_id;
                ++TC;
                string args = "(";
                if (TF == LP) {
                    ++TC;
                    while(TF != RP) {
                        if (TF == COM || TF == SIM) {
                            args += ", ";
                            ++TC;
                        }
                        bool ref = false;
                        if (TF == VAR) {
                            ref = true;
                            ++TC;
                        }
                        Vars var = get_def();
                        if (!var.d) {
                            string id = var.id[0];
                            if (st.find(id) != st.end()) {
                                error();
                            }
                            args += id_map_rev[var.type].c_str();
                            if (ref) {
                                args += "&";
                            }
                            args += " ";
                            args += id.c_str();
                            vector<int> tmp;
                            st[id] = tmp;
                            int sz = var.id.size();
                            for (int i = 1; i < sz; ++i) {
                                id = var.id[i];
                                args += ", ";
                                args += id_map_rev[var.type].c_str();
                                if (ref) {
                                    args += "&";
                                }
                                args += " ";
                                args += id.c_str();
                                st[id] = tmp;
                            }
                        }
                    }
                    ++TC;
                }
                args += ")";
                if (type == 1) {
                    if (TF != COL) {
                        error();
                    } else {
                        ++TC;
                        ret_type = id_map_rev[TS];
                        fprintf(out_file, "%s %s%s", ret_type.c_str(), fun_id.c_str(), args.c_str());
                        ++TC;
                    }
                } else {
                    fprintf(out_file, "void %s%s", fun_id.c_str(), args.c_str());
                }
            } else {
                error();
            }
            while (TF == SIM) {
                ++TC;
            }
            fprintf(out_file, "\n{\n");
            indent_depth = 1;
            string tmp = "";
            indent(tmp);
            if (type) {
                fprintf(out_file, "%s%s result;\n", tmp.c_str(), ret_type.c_str());
            }
            if (TF == VAR) {
                ++TC;
                build_st(1);
            }
            ++TC;
            while (TF != END) {
                string ret = match_statement();
                fprintf(out_file, "%s", ret.c_str());
            }
            if (type) {
                fprintf(out_file, "%sreturn result;\n", tmp.c_str());
            }
            fprintf(out_file, "}\n");
            ++TC;
            while (TF == SIM) {
                ++TC;
            }
        }
    }/*}}}*/
    void match_main() {/*{{{*/
        fprintf(out_file, "int main()\n");
        fprintf(out_file, "{\n");
        indent_depth = 1;
        ++TC;
        string tmp = "";
        indent(tmp);
        fprintf(out_file, "%sstd::ios::sync_with_stdio(false);\n", tmp.c_str());
        while (TF != END) {
            string ret = match_statement();
            fprintf(out_file, "%s", ret.c_str());
        }
        for (int i = 0; i < INDENT_STEP; ++i) {
            fprintf(out_file, " ");
        }
        fprintf(out_file, "return 0;\n");
        fprintf(out_file, "}\n\n");
    }/*}}}*/
    void sys_func_gen() {/*{{{*/
        fprintf(out_file, "int ord(char x)\n{\n");
        for (int i = 0; i < INDENT_STEP; ++i) {
            fprintf(out_file, " ");
        }
        fprintf(out_file, "return (int)x;\n}\n");
        fprintf(out_file, "bool odd(int x)\n{\n");
        for (int i = 0; i < INDENT_STEP; ++i) {
            fprintf(out_file, " ");
        }
        fprintf(out_file, "return (bool)(x & 1);\n}\n");
        fprintf(out_file, "bool even(int x)\n{\n");
        for (int i = 0; i < INDENT_STEP; ++i) {
            fprintf(out_file, " ");
        }
        fprintf(out_file, "return !odd(x);\n}\n");
        fprintf(out_file, "char chr(int x)\n{\n");
        for (int i = 0; i < INDENT_STEP; ++i) {
            fprintf(out_file, " ");
        }
        fprintf(out_file, "return (char)x;\n}\n");
        fprintf(out_file, "int length(const string& x)\n{\n");
        for (int i = 0; i < INDENT_STEP; ++i) {
            fprintf(out_file, " ");
        }
        fprintf(out_file, "return (int)x.length();\n}\n");
        fprintf(out_file, "void inc(long long& x, long long y = 1)\n{\n");
        for (int i = 0; i < INDENT_STEP; ++i) {
            fprintf(out_file, " ");
        }
        fprintf(out_file, "x += y;\n}\n");
        fprintf(out_file, "void dec(long long& x, long long y = 1)\n{\n");
        for (int i = 0; i < INDENT_STEP; ++i) {
            fprintf(out_file, " ");
        }
        fprintf(out_file, "x -= y;\n}\n");
    }/*}}}*/
    void match() {/*{{{*/
        indent_depth = 0;
        TC = tokens.begin();
        if (TF == PROG) {
            ++TC;
            ++TC;
            while (TF == SIM) {
                ++TC;
            }
        }
        if (TF == VAR) {
            ++TC;
            build_st();
        }
        sys_func_gen();
        match_function();
        match_main();
    }/*}}}*/
    public:
    void run(FILE* in, FILE* out) {/*{{{*/
        fun_id_cur = "main";
        out_file = out;
        init();
        read_in(in);
        to_tokens();
        match();
    }/*}}}*/
};
int main(int argc, char* argv[]) {/*{{{*/
    if (argc != 3) {
        fprintf(stderr, "usage: ./main <input_file> <output_file>\n");
        return -1;
    }
    FILE* in = fopen(argv[1], "r");
    FILE* out = fopen(argv[2], "w");
    Parser parser;
    parser.run(in, out);
    return 0;
}/*}}}*/

