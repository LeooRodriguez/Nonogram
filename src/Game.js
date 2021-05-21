import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.restart(true)
  }



  handlePengineCreate() {
    const queryS = 'init(PistasFilas, PistasColumns, Grilla)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grilla'],
          rowClues: response['PistasFilas'],
          colClues: response['PistasColumns'],
          satisfaccionFil: Array(response['PistasFilas'].length).fill(false), //inicializamos la fila para verificar en falso
          satisfaccionCol: Array(response['PistasColumns'].length).fill(false) //inicializamos la columna para verificar en falso
        });
      }
    });
  }

  handleClick(i, j) {
    // No action on click if we are waiting.
    if (this.state.waiting) {
      return;
    }
    // Build Prolog query to make the move, which will look as follows:
    // put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    const squaresS = JSON.stringify(this.state.grid).replaceAll('"_"', "_"); // Remove quotes for variables.
    const modo = this.state.modoBoton;
    const rowClue = JSON.stringify(this.state.rowClues);
    const colClue = JSON.stringify(this.state.colClues);
    const queryS = 'put("' + modo + '", [' + i + ',' + j + ']' + ', ' + rowClue + ', ' + colClue + ',' + squaresS + ', GrillaRes, FilaSat, ColSat)';
    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      if (success) {
        let newGrid = response['GrillaRes'];
        let satisfaceFil = response['FilaSat'];
        let satisfaceCol = response['ColSat'];
        this.setState({
          grid: newGrid
        });
        this.confirmarFilaSatisfecha(i, satisfaceFil === 1);
        this.confirmarColSatisfecha(j, satisfaceCol === 1);
      } else {
        this.setState({
          waiting: false
        });
      }
    });
  }

  /**
   * Verifica si la columna con el indice index verifica la propiedad.
   * @param {*} index es el indice de la columna que deseamos verificar 
   * @param {*} satisface indica si se satisfacio la columna 
   */
  confirmarColSatisfecha(index, satisface) {
    let satisfaccionCol = [...this.state.satisfaccionCol];
    satisfaccionCol[index] = satisface;
    this.setState({ satisfaccionCol, waiting: false });
  }

  /**
   * Verifica si la fila con el indice index verifica la propiedad.
   * @param {*} index es el indice de la fila que deseamos verificar 
   * @param {*} satisface indica si se satisfacio la fila
   */

  confirmarFilaSatisfecha(index, satisface) {
    let satisfaccionFil = [...this.state.satisfaccionFil];
    satisfaccionFil[index] = satisface;
    this.setState({ satisfaccionFil, waiting: false });
  }
  /**
   * Cambia el estado del boton, alternando entre el # y la X según cual sea su estado actual.
   */
  cambiar() {
    if (this.state.modoBoton === "#") {
      this.setState({ modoBoton: "X" })
    }
    else
      this.setState({ modoBoton: "#" })
  }

  abrirManual() {
    window.open("https://es.puzzle-nonograms.com/faq.php", "Manual de usuario / Tutorial", "width=800, height=700")
  }
  irGithub() {
    window.open("https://github.com/LeooRodriguez/Nonogram", "GitHub", "width=800, height=700")
  }


  render() {
    if (this.state.grid === null) {
      return null;
    }



    let { satisfaccionFil, satisfaccionCol } = this.state;

    if (satisfaccionFil.every(a => a) && satisfaccionCol.every(a => a)) {//Si todas las filas y columnas de la grilla son true --> ganaste el juego.
      return <div>
        <div className={"cartelGanaste"}>¡FELICIDADES,HAS GANADO!</div>

        <button className={"botonDeReset"} onClick={() => this.restart()}>
          Pulse para comenzar otra partida
        </button>

        <button className={"botonDeGit"} onClick={() => this.irGithub()}>
          GitHub
        </button>
      </div>
    }


    return (
      <div className="game">
        <Board
          grid={this.state.grid}
          rowClues={this.state.rowClues}
          colClues={this.state.colClues}
          onClick={(i, j) => this.handleClick(i, j)}
          satisfaccionFil={this.state.satisfaccionFil}
          satisfaccionCol={this.state.satisfaccionCol}
        />

        <button className="box clue nsat" onClick={() => this.cambiar()} >
          {this.state.modoBoton}
        </button>
        <div>
          <div className={"cartelNombre"}>Nonograma
        </div>
          <div>
            <div className={"Estado"}>Modo:
        </div>
          </div>
        </div>
        <button className={"botonReglas"} onClick={() => this.abrirManual()}>
          Reglas
        </button>
      </div>

    );
  }
  restart(isFirstTime = false) {
    this.state = {
      grid: null,
      rowClues: null,
      colClues: null,
      waiting: false,
      satisfaccionFil: [],//Guarda verdaderos o falsos dependiendo si la filas cumple las propiedades del nonograma. 
      satisfaccionCol: [],//Guarda verdaderos o falsos dependiendo si la columnas cumple las propiedades del nonograma.
      modoBoton: "#",//Verifica en que estado esta el botón ( # ó X).
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    if (isFirstTime) {
      this.pengine = new PengineClient(this.handlePengineCreate);
    }
    else {
      this.handlePengineCreate()//Inicia una nueva partida ya mismo.
    }
  }
}



export default Game;


