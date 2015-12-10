using UnityEngine;
using System.Collections;
using UnityEngine.UI;

public class Scoreboard : MonoBehaviour {

    int scoreA, scoreB;

    public Text textA, textB;

	// Use this for initialization
	void Awake () {
        scoreA = 0;
        scoreB = 0;
	}
	
    public void ScoreA() { textA.text = (++scoreA).ToString(); }
    public void ScoreB() { textB.text = (++scoreB).ToString(); }
}
