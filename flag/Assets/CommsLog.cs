using UnityEngine;
using System.Collections;
using UnityEngine.UI;

public class CommsLog : MonoBehaviour {

    public Text text;

    const uint num_lines = 20;
    string[] lines = new string[num_lines];
    uint top = 0;
    uint bottom = 0;

    uint Plus(uint i) { return (i + 1) % num_lines; }
    uint Minus(uint i) { return (i - 1) % num_lines; }

	public void AddLine(string line)
    {
        bottom = Plus(bottom);
        if (Plus(bottom) == top)
            top = Plus(top);
        lines[bottom] = line;

        string s = "";
        for (uint i = top; i != bottom; i = Plus(i))
            s += lines[i] + "\n";
        text.text = s;
    }
}
