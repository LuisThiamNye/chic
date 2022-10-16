package tmp;

class Tmp {

	static void main(int c) throws Exception {
		switch (c) {
			case '"':
			case ';':
			case '\'':
			case '^':
			case '(':
			case ')':
			case '[':
			case ']':
			case '{':
			case '}':
			case '\\':
			case '#':
				var x = -1;
			default:
				return;
		}
	}
}