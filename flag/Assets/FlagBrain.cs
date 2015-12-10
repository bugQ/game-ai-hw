using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System;

public class FlagBrain : MonoBehaviour {
    public enum Team { A, B }
    public enum Info
    {
        OffSides,
        Captured,
        InJail,
        HasFlag,
        MustRescue1,
        MustRescue2,
        MustRescue3,
        MustCapture1,
        MustCapture2,
        MustCapture3,
        MustProtect1,
        MustProtect2,
        MustProtect3,
    }

    public Team team { get; private set; }
    public uint player_number { get; private set; }
    Team enemy_team;

    CommsLog comms_log;
    Scoreboard scoreboard;

    Dictionary<Info, bool> knowledge = new Dictionary<Info, bool>();
    public bool this[Info flag]
    {
        get { return knowledge[flag]; }

        private set
        {
            knowledge[flag] = value;

            Debug.Log(this.name + "[" + flag + "] = " + value);
        }
    }

    private void Broadcast(Info flag, bool value)
    {
        comms_log.AddLine(this.name + ": " + flag + " " + (value ? "ON !" : "OFF !"));

        foreach ( GameObject mate in GameObject.FindGameObjectsWithTag("team" + team) )
            if(mate != this.gameObject)
                mate.GetComponent<FlagBrain>()[flag] = value;
    }

    // hack, shouldn't do this
    private void BroadcastEnemy(Info flag, bool value)
    {
        foreach (GameObject mate in GameObject.FindGameObjectsWithTag("team" + enemy_team))
            if (mate != this.gameObject)
            {
                comms_log.AddLine(mate.name + ": " + flag + (value ? "ON !" : "OFF !"));
                mate.GetComponent<FlagBrain>()[flag] = value;
            }
    }

    void Awake()
    {
        Material mat = GetComponent<MeshRenderer>().material;

        switch (mat.name.Split(' ')[0])
        {
            case "azure":
                team = Team.A;
                enemy_team = Team.B;
                break;
            case "violet":
                team = Team.B;
                enemy_team = Team.A;
                break;
        }

        player_number = uint.Parse(this.name[this.name.Length - 1].ToString());

        foreach ( Info flag in Enum.GetValues(typeof(Info)) )
        {
            this[flag] = false;
        }

        var canvas = GameObject.Find("Canvas");
        comms_log = canvas.GetComponent<CommsLog>();
        scoreboard = canvas.GetComponent<Scoreboard>();
    }

    void Start()
    {
        this.GetComponent<Rigidbody>().AddForce(5 * new Vector3(-6, 8, 0));
    }

    void OnTriggerEnter(Collider other)
    {
        if (other.name == "zone" + team)
        {
            this[Info.OffSides] = false;
            BroadcastEnemy((Info)Enum.Parse(typeof(Info), "MustCapture" + player_number), false);

            if (this[Info.HasFlag])
            {
                this[Info.HasFlag] = false;
                Broadcast((Info)Enum.Parse(typeof(Info), "MustProtect" + player_number), false);
                switch (team)
                {
                    case Team.A:
                        scoreboard.ScoreA();
                        break;
                    case Team.B:
                        scoreboard.ScoreB();
                        break;
                }
            }
        }

        if (this[Info.OffSides] && other.name == "jail" + enemy_team)
        {
            this[Info.InJail] = true;
            Broadcast((Info)Enum.Parse(typeof(Info), "MustRescue" + player_number), true);
        }

        if (!this[Info.Captured] && other.name == "flag" + enemy_team)
        {
            this[Info.HasFlag] = true;
            Broadcast((Info)Enum.Parse(typeof(Info), "MustProtect" + player_number), true);
        }
    }

    void OnTriggerExit(Collider other)
    {
        if (other.name == "zone" + team)
        {
            this[Info.OffSides] = true;
            BroadcastEnemy((Info)Enum.Parse(typeof(Info), "MustCapture" + player_number), true);
        }
        
    }

    void OnCollisionEnter(Collision c)
    {
        Collider other = c.collider;

        if (this[Info.OffSides] && other.name.StartsWith("mate" + enemy_team))
        {
            this[Info.Captured] = true;
            BroadcastEnemy((Info)Enum.Parse(typeof(Info), "MustCapture" + player_number), false);
        }

        if (this[Info.InJail] && other.name.StartsWith("mate" + team)
            && !c.collider.GetComponent<FlagBrain>()[Info.Captured])
        {
            this[Info.Captured] = false;
            Broadcast((Info)Enum.Parse(typeof(Info), "MustRescue" + player_number), false);
        }
    }

    // Update is called once per frame
    void Update () {

	}
}
