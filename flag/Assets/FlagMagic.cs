using UnityEngine;
using System.Collections;

public class FlagMagic : MonoBehaviour {
    Vector3 starting_pos;

	// Use this for initialization
	void Awake ()
    {
        starting_pos = transform.position;
	}
	
    public void Reset()
    {
        transform.position = starting_pos;
    }
}
